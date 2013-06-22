#include "hsqml.h"
#include "HsQMLManager.h"
#include "HsQMLEngine.h"
#include "HsQMLObject.h"
#include "HsQMLWindow.h"

HsQMLEngine::HsQMLEngine(HsQMLEngineConfig& config)
{
    // Obtain, re-parent, and set QML global object
    if (config.contextObject) {
        QObject* contextObject = config.contextObject->object();
        contextObject->setParent(this);
        mEngine.rootContext()->setContextObject(contextObject);
    }

    // Create window
    HsQMLWindow* win = new HsQMLWindow(this);
    win->setParent(this);
    win->setSource(config.initialURL);
    win->setVisible(config.showWindow);
    if (config.setWindowTitle) {
        win->setTitle(config.windowTitle);
    }
}

HsQMLEngine::~HsQMLEngine()
{
}

void HsQMLEngine::childEvent(QChildEvent* ev)
{
    if (ev->removed() && children().size() == 0) {
        deleteLater();
    }
}

QDeclarativeEngine* HsQMLEngine::engine()
{
    return &mEngine;
}

extern "C" void hsqml_create_engine(
    HsQMLObjectHandle* contextObject,
    HsQMLUrlHandle* initialURL,
    int showWindow,
    int setWindowTitle,
    HsQMLStringHandle* windowTitle)
{
    HsQMLEngineConfig config;
    config.contextObject = reinterpret_cast<HsQMLObjectProxy*>(contextObject);
    config.initialURL = *reinterpret_cast<QUrl*>(initialURL);
    config.showWindow = static_cast<bool>(showWindow);
    if (setWindowTitle) {
        config.setWindowTitle = true;
        config.windowTitle = *reinterpret_cast<QString*>(windowTitle);
    }

    Q_ASSERT (gManager);
    QMetaObject::invokeMethod(
        gManager, "createEngine", Qt::QueuedConnection,
        Q_ARG(HsQMLEngineConfig, config));
}
