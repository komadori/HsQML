#include <iostream>
#include <cstdlib>
#include <QtCore/QMetaType>
#include <QtCore/QThread>

#include "HsQMLManager.h"

QAtomicPointer<HsQMLManager> gManager;

HsQMLManager::HsQMLManager(
    void (*freeFun)(HsFunPtr),
    void (*freeStable)(HsStablePtr))
    : mLogLevel(0)
    , mFreeFun(freeFun)
    , mFreeStable(freeStable)
    , mAppRunning(false)
{
    qRegisterMetaType<HsQMLEngineConfig>("HsQMLEngineConfig");

    const char* env = std::getenv("HSQML_DEBUG_LOG_LEVEL");
    if (env) {
        mLogLevel = QString(env).toInt();
    }
}

void HsQMLManager::setLogLevel(int ll)
{
    mLogLevel = ll;
}

bool HsQMLManager::checkLogLevel(int ll)
{
    return mLogLevel >= ll;
}

void HsQMLManager::log(const QString& msg)
{
    std::cerr << "HsQML: " << msg.toStdString() << std::endl;
}

void HsQMLManager::freeFun(HsFunPtr funPtr)
{
    mFreeFun(funPtr);
}

void HsQMLManager::freeStable(HsStablePtr stablePtr)
{
    mFreeStable(stablePtr);
}

int HsQMLManager::startEngine(const HsQMLEngineConfig& config)
{
    // Initialise application
    bool usingThread = false;
    mAppLock.lockForWrite();
    if (mApp && !mAppRunning && mApp->thread() != QThread::currentThread()) {
        mAppLock.unlock();
        return -1;
    }
    if (!mApp) {
        mApp = new HsQMLManagerApp();
    }
    if (!mAppRunning) {
        usingThread = true;
        mAppRunning = true;
    }

    // Create engine
    QMetaObject::invokeMethod(
        mApp, "createEngine", Q_ARG(HsQMLEngineConfig, config));
    mAppLock.unlock();

    // Run event loop if neccessary
    if (usingThread) {
        if (mApp->exec() == 0) {
            mAppRunning = false;
            // Lock was acquired during event loop exit
            mAppLock.unlock();
        }
        else {
            // Error while entering loop
            mAppRunning = false;
            return -1;
        }
    }
    return 0;
}

HsQMLManagerApp::HsQMLManagerApp()
    : mArgC(1)
    , mArg0(0)
    , mArgV(&mArg0)
    , mApp(mArgC, &mArgV)
{
    mApp.setQuitOnLastWindowClosed(false);
}

HsQMLManagerApp::~HsQMLManagerApp()
{}

void HsQMLManagerApp::childEvent(QChildEvent* ev)
{
    if (ev->removed() && children().size() == 0) {
        gManager->mAppLock.lockForWrite();
        // Allow events to flush out of queue
        QCoreApplication::postEvent(
            this, new QEvent(QEvent::User), Qt::LowEventPriority);
    }
}

void HsQMLManagerApp::customEvent(QEvent* ev)
{
    if (ev->type() == QEvent::User) {
        if (children().size() == 0) {
            mApp.quit();
        }
        else {
            // New engines have started
            gManager->mAppLock.unlock();
        }
    }
}

void HsQMLManagerApp::createEngine(HsQMLEngineConfig config)
{
    HsQMLEngine* engine = new HsQMLEngine(config);
    engine->setParent(this);
}

int HsQMLManagerApp::exec()
{
    return mApp.exec();
}

extern "C" void hsqml_init(
    void (*freeFun)(HsFunPtr),
    void (*freeStable)(HsStablePtr))
{
    if (gManager == NULL) {
        HsQMLManager* manager = new HsQMLManager(freeFun, freeStable);
        if (!gManager.testAndSetOrdered(NULL, manager)) {
            delete manager;
        }
    }
}

extern "C" void hsqml_set_debug_loglevel(int ll)
{
    Q_ASSERT (gManager);
    gManager->setLogLevel(ll);
}
