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
        QObject* ctx = config.contextObject->object(this);
        mEngine.rootContext()->setContextObject(ctx);
        mObjects << ctx;
    }

    // Load document
    mComponent.loadUrl(QUrl(config.initialURL));
}

HsQMLEngine::~HsQMLEngine()
{
    // Call stop callback
    mStopCb();
    gManager->freeFun(reinterpret_cast<HsFunPtr>(mStopCb));

    // Delete owned objects
    qDeleteAll(mObjects);
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
        mObjects << obj;
        QQuickWindow* win = qobject_cast<QQuickWindow*>(obj);
        QQuickItem* item = qobject_cast<QQuickItem*>(obj);
        if (item) {
            win = new QQuickWindow();
            mObjects << win;
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
    }
}

extern "C" void hsqml_create_engine(
    HsQMLObjectHandle* contextObject,
    HsQMLStringHandle* initialURL,
    HsQMLTrivialCb stopCb)
{
    HsQMLEngineConfig config;
    config.contextObject = reinterpret_cast<HsQMLObjectProxy*>(contextObject);
    config.initialURL = *reinterpret_cast<QString*>(initialURL);
    config.stopCb = stopCb;

    Q_ASSERT (gManager);
    gManager->createEngine(config);
}
