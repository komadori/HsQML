#include <QtQuick/QQuickItem>
#include <QtQuick/QQuickWindow>

#include "Manager.h"
#include "Engine.h"
#include "Object.h"

static const char* cRefSrcNames[] = {
    "Hndl", "Eng", "Event"
};

HsQMLEngineProxy::HsQMLEngineProxy()
    : mEngine(NULL)
    , mDead(false)
    , mSerial(gManager->updateCounter(HsQMLManager::EngineSerial, 1))
    , mRefCount(0)
{
    ref(Handle);
    gManager->updateCounter(HsQMLManager::EngineCount, 1);
}

HsQMLEngineProxy::~HsQMLEngineProxy()
{
    gManager->updateCounter(HsQMLManager::EngineCount, -1);
}

void HsQMLEngineProxy::setEngine(HsQMLEngine* engine)
{
    mEngine = engine;
}

HsQMLEngine* HsQMLEngineProxy::engine() const
{
    return mEngine;
}

void HsQMLEngineProxy::kill()
{
    mDead = true;
    delete mEngine;
    Q_ASSERT (!mEngine);
}

bool HsQMLEngineProxy::dead() const
{
    return mDead;
}

void HsQMLEngineProxy::ref(RefSrc src)
{
    int count = mRefCount.fetchAndAddOrdered(1);

    HSQML_LOG(count == 0 ? 3 : 4,
        QString().sprintf("%s EngineProxy, id=%d, src=%s, count=%d.",
        count ? "Ref" : "New", mSerial, cRefSrcNames[src], count+1));
}

void HsQMLEngineProxy::deref(RefSrc src)
{
    int count = mRefCount.fetchAndAddOrdered(-1);

    HSQML_LOG(count == 0 ? 3 : 4,
        QString().sprintf("%s EngineProxy, id=%d, src=%s, count=%d.",
        count > 1 ? "Deref" : "Delete", mSerial, cRefSrcNames[src], count));

    if (count == 1) {
        delete this;
    }
}

HsQMLEngineCreateEvent::HsQMLEngineCreateEvent(HsQMLEngineProxy* proxy)
    : QEvent(HsQMLManagerApp::CreateEngineEvent)
    , mProxy(proxy)
    , contextObject(NULL)
    , stopCb(NULL)
{
    mProxy->ref(HsQMLEngineProxy::Event);
}

HsQMLEngineProxy* HsQMLEngineCreateEvent::proxy() const
{
    return mProxy;
}

HsQMLEngineCreateEvent::~HsQMLEngineCreateEvent()
{
    mProxy->deref(HsQMLEngineProxy::Event);
}

HsQMLEngine::HsQMLEngine(const HsQMLEngineCreateEvent* config, QObject* parent)
    : QObject(parent) 
    , mProxy(config->proxy())
    , mComponent(&mEngine)
    , mStopCb(config->stopCb)
{
    // Setup life-cycle
    mProxy->setEngine(this);
    mProxy->ref(HsQMLEngineProxy::Engine);

    // Connect signals
    QObject::connect(
        &mEngine, SIGNAL(quit()),
        this, SLOT(deleteLater()));
    QObject::connect(
        &mComponent, SIGNAL(statusChanged(QQmlComponent::Status)),
        this, SLOT(componentStatus(QQmlComponent::Status)));

    // Obtain, re-parent, and set QML global object
    if (config->contextObject) {
        HsQMLObjectProxy* ctxProxy = config->contextObject;
        ctxProxy->ref(HsQMLObjectProxy::Engine);
        mGlobals << ctxProxy;
        mEngine.rootContext()->setContextObject(ctxProxy->object(this));
    }

    // Engine settings
    mEngine.setImportPathList(
        QStringList(config->importPaths) << mEngine.importPathList());
    mEngine.setPluginPathList(
        QStringList(config->pluginPaths) << mEngine.pluginPathList());

    // Load document
    mComponent.loadUrl(QUrl(config->initialURL));
}

HsQMLEngine::~HsQMLEngine()
{
    // Call stop callback
    mStopCb();
    gManager->freeFun(reinterpret_cast<HsFunPtr>(mStopCb));

    // Release engine proxy and globals
    mProxy->setEngine(NULL);
    mProxy->deref(HsQMLEngineProxy::Engine);
    Q_FOREACH(HsQMLObjectProxy* proxy, mGlobals) {
        proxy->deref(HsQMLObjectProxy::Engine);
    }

    // Delete other owned resources
    qDeleteAll(mResources);
}

bool HsQMLEngine::eventFilter(QObject* obj, QEvent* ev)
{
    if (QEvent::Close == ev->type()) {
        deleteLater();
    }
    return false;
}

QQmlEngine* HsQMLEngine::declEngine()
{
    return &mEngine;
}

void HsQMLEngine::componentStatus(QQmlComponent::Status status)
{
    switch (status) {
    case QQmlComponent::Ready: {
        QObject* obj = mComponent.create();
        // Freeing the object causes memory corruption prior to Qt 5.2
#if QT_VERSION >= 0x050200
        mResources << obj;
#endif
        QQuickWindow* win = qobject_cast<QQuickWindow*>(obj);
        QQuickItem* item = qobject_cast<QQuickItem*>(obj);
        if (item) {
            win = new QQuickWindow();
            mResources << win;
            item->setParentItem(win->contentItem());
            int width = item->width();
            int height = item->height();
            if (width < 1 || height < 1) {
                width = item->implicitWidth();
                height = item->implicitHeight();
            }
            win->setWidth(width);
            win->setHeight(height);
            win->contentItem()->setWidth(width);
            win->contentItem()->setHeight(height);
            win->setTitle("HsQML Window");
            win->show();
        }
        if (win) {
            win->installEventFilter(this);
            mEngine.setIncubationController(win->incubationController());
        }
        break;}
    case QQmlComponent::Error: {
        QList<QQmlError> errs = mComponent.errors();
        for (QList<QQmlError>::iterator it = errs.begin();
             it != errs.end(); ++it) {
            HSQML_LOG(0, it->toString());
        }
        deleteLater();
        break;}
    default: break;
    }
}

extern "C" HsQMLEngineHandle* hsqml_create_engine(
    HsQMLObjectHandle* contextObject,
    HsQMLStringHandle* initialURL,
    HsQMLStringHandle** importPaths,
    HsQMLStringHandle** pluginPaths,
    HsQMLTrivialCb stopCb)
{
    Q_ASSERT (gManager);

    HsQMLEngineProxy* proxy = new HsQMLEngineProxy();
    HsQMLEngineCreateEvent* config = new HsQMLEngineCreateEvent(proxy);

    config->contextObject = reinterpret_cast<HsQMLObjectProxy*>(contextObject);
    config->initialURL = *reinterpret_cast<QString*>(initialURL);
    for (QString** p = reinterpret_cast<QString**>(importPaths); *p; p++) {
        config->importPaths.push_back(**p);
    }
    for (QString** p = reinterpret_cast<QString**>(pluginPaths); *p; p++) {
        config->pluginPaths.push_back(**p);
    }
    config->stopCb = stopCb;

    gManager->postAppEvent(config);
    return reinterpret_cast<HsQMLEngineHandle*>(proxy);
}

extern "C" void hsqml_kill_engine(
    HsQMLEngineHandle* hndl)
{
    HsQMLEngineProxy* proxy = reinterpret_cast<HsQMLEngineProxy*>(hndl);

    Q_ASSERT(gManager->isEventThread());
    proxy->kill();
}

extern void hsqml_finalise_engine_handle(
    HsQMLEngineHandle* hndl)
{
    HsQMLEngineProxy* proxy = (HsQMLEngineProxy*)hndl;
    proxy->deref(HsQMLEngineProxy::Handle);
}
