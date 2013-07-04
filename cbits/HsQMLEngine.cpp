#include "HsQMLManager.h"
#include "HsQMLEngine.h"
#include "HsQMLObject.h"
#include "HsQMLWindow.h"

HsQMLEngine::HsQMLEngine(const HsQMLEngineConfig& config)
    : mStopCb(config.stopCb)
{
    // Obtain, re-parent, and set QML global object
    if (config.contextObject) {
        mContextObj.reset(config.contextObject->object());
        mEngine.rootContext()->setContextObject(mContextObj.data());
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
    // Call stop callback
    mStopCb();
    gManager->freeFun(reinterpret_cast<HsFunPtr>(mStopCb));
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

extern "C" int hsqml_run_engine(
    HsQMLObjectHandle* contextObject,
    HsQMLUrlHandle* initialURL,
    int showWindow,
    int setWindowTitle,
    HsQMLStringHandle* windowTitle,
    HsQMLEngineStopCb stopCb)
{
    HsQMLEngineConfig config;
    config.contextObject = reinterpret_cast<HsQMLObjectProxy*>(contextObject);
    config.initialURL = *reinterpret_cast<QUrl*>(initialURL);
    config.showWindow = static_cast<bool>(showWindow);
    if (setWindowTitle) {
        config.setWindowTitle = true;
        config.windowTitle = *reinterpret_cast<QString*>(windowTitle);
    }
    config.stopCb = stopCb;

    Q_ASSERT (gManager);
    return gManager->startEngine(config);
}
