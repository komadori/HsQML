#include <QtQuick/QQuickItem>
#include <QtQuick/QQuickWindow>

#include "Manager.h"
#include "Engine.h"
#include "Object.h"

HsQMLEngine::HsQMLEngine(const HsQMLEngineConfig& config)
    : mComponent(&mEngine)
    , mStopCb(config.stopCb)
{
    // Connect signals
    QObject::connect(
        &mEngine, SIGNAL(quit()),
        this, SLOT(deleteLater()));
    QObject::connect(
        &mComponent, SIGNAL(statusChanged(QQmlComponent::Status)),
        this, SLOT(componentStatus(QQmlComponent::Status)));

    // Obtain, re-parent, and set QML global object
    if (config.contextObject) {
        HsQMLObjectProxy* ctxProxy = config.contextObject;
        ctxProxy->ref(HsQMLObjectProxy::Engine);
        mGlobals << ctxProxy;
        mEngine.rootContext()->setContextObject(ctxProxy->object(this));
    }

    // Engine settings
    mEngine.setImportPathList(
        QStringList(config.importPaths) << mEngine.importPathList());
    mEngine.setPluginPathList(
        QStringList(config.pluginPaths) << mEngine.pluginPathList());

    // Load document
    mComponent.loadUrl(QUrl(config.initialURL));
}

HsQMLEngine::~HsQMLEngine()
{
    // Call stop callback
    mStopCb();
    gManager->freeFun(reinterpret_cast<HsFunPtr>(mStopCb));

    // Release globals
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

extern "C" void hsqml_create_engine(
    HsQMLObjectHandle* contextObject,
    HsQMLStringHandle* initialURL,
    HsQMLStringHandle** importPaths,
    HsQMLStringHandle** pluginPaths,
    HsQMLTrivialCb stopCb)
{
    HsQMLEngineConfig config;
    config.contextObject = reinterpret_cast<HsQMLObjectProxy*>(contextObject);
    config.initialURL = *reinterpret_cast<QString*>(initialURL);
    for (QString** p = reinterpret_cast<QString**>(importPaths); *p; p++) {
        config.importPaths.push_back(**p);
    }
    for (QString** p = reinterpret_cast<QString**>(pluginPaths); *p; p++) {
        config.pluginPaths.push_back(**p);
    }
    config.stopCb = stopCb;

    Q_ASSERT (gManager);
    gManager->createEngine(config);
}
