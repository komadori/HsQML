#include <iostream>
#include <cstdlib>
#include <QtCore/QBasicTimer>
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
    , mLock(QMutex::Recursive)
    , mRunning(false)
    , mRunCount(0)
    , mStartCb(NULL)
    , mJobsCb(NULL)
    , mYieldCb(NULL)
    , mActiveEngine(NULL)
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

bool HsQMLManager::isEventThread()
{
    return mApp && mApp->thread() == QThread::currentThread();
}

HsQMLManager::EventLoopStatus HsQMLManager::runEventLoop(
    HsQMLTrivialCb startCb,
    HsQMLTrivialCb jobsCb,
    HsQMLTrivialCb yieldCb)
{
    QMutexLocker locker(&mLock);

    // Check if already running
    if (mRunning) {
        return HSQML_EVLOOP_ALREADY_RUNNING;
    }

    // Check if event loop bound to a different thread
    if (mApp && !isEventThread()) {
        return HSQML_EVLOOP_WRONG_THREAD;
    }

    // Create application object
    if (!mApp) {
        mApp = new HsQMLManagerApp();
    }

    // Save callbacks
    mStartCb = startCb;
    mJobsCb = jobsCb;
    mYieldCb = yieldCb;

    // Setup events
    QCoreApplication::postEvent(
        mApp, new QEvent(HsQMLManagerApp::StartedLoopEvent),
        Qt::HighEventPriority);
    QBasicTimer idleTimer;
    if (yieldCb) {
        idleTimer.start(0, mApp);
    }

    // Run loop
    int ret = mApp->exec();

    // Cleanup callbacks
    freeFun(startCb);
    mStartCb = NULL;
    freeFun(jobsCb);
    mJobsCb = NULL;
    if (yieldCb) {
        freeFun(yieldCb);
        mYieldCb = NULL;
    }

    // Return
    if (ret == 0) {
        return HSQML_EVLOOP_OK;
    }
    else {
        QCoreApplication::removePostedEvents(
            mApp, HsQMLManagerApp::StartedLoopEvent);
        return HSQML_EVLOOP_OTHER_ERROR;
    }
}

HsQMLManager::EventLoopStatus HsQMLManager::requireEventLoop()
{
    QMutexLocker locker(&mLock);
    if (mRunCount > 0) {
        mRunCount++;
        return HSQML_EVLOOP_OK;
    }
    else {
        return HSQML_EVLOOP_NOT_RUNNING;
    }
}

void HsQMLManager::releaseEventLoop()
{
    QMutexLocker locker(&mLock);
    if (--mRunCount == 0) {
        QCoreApplication::postEvent(
            mApp, new QEvent(HsQMLManagerApp::StopLoopEvent),
            Qt::LowEventPriority);
    }
}

void HsQMLManager::notifyJobs()
{
    QMutexLocker locker(&mLock);
    if (mRunCount > 0) {
        QCoreApplication::postEvent(
            mApp, new QEvent(HsQMLManagerApp::PendingJobsEvent));
    }
}

void HsQMLManager::createEngine(const HsQMLEngineConfig& config)
{
    Q_ASSERT (mApp);
    QMetaObject::invokeMethod(
        mApp, "createEngine", Q_ARG(HsQMLEngineConfig, config));
}

void HsQMLManager::setActiveEngine(HsQMLEngine* engine)
{
    Q_ASSERT(!mActiveEngine || !engine);
    mActiveEngine = engine;
}

HsQMLEngine* HsQMLManager::activeEngine()
{
    return mActiveEngine;
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

void HsQMLManagerApp::customEvent(QEvent* ev)
{
    switch (ev->type()) {
    case HsQMLManagerApp::StartedLoopEvent: {
        gManager->mRunning = true;
        gManager->mRunCount++;
        gManager->mLock.unlock();
        gManager->mStartCb();
        gManager->mJobsCb();
        break;}
    case HsQMLManagerApp::StopLoopEvent: {
        gManager->mLock.lock();
        const QObjectList& cs = gManager->mApp->children();
        while (!cs.empty()) {
            delete cs.front();
        }
        gManager->mRunning = false;
        gManager->mApp->mApp.quit();
        break;}
    case HsQMLManagerApp::PendingJobsEvent: {
        gManager->mJobsCb();
        break;}
    }
}

void HsQMLManagerApp::timerEvent(QTimerEvent*)
{
    Q_ASSERT(gManager->mYieldCb);
    gManager->mYieldCb();
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

extern "C" HsQMLEventLoopStatus hsqml_evloop_run(
    HsQMLTrivialCb startCb,
    HsQMLTrivialCb jobsCb,
    HsQMLTrivialCb yieldCb)
{
    return gManager->runEventLoop(startCb, jobsCb, yieldCb);
}

extern "C" HsQMLEventLoopStatus hsqml_evloop_require()
{
    return gManager->requireEventLoop();
}

extern "C" void hsqml_evloop_release()
{
    gManager->releaseEventLoop();
}

extern "C" void hsqml_evloop_notify_jobs()
{
    gManager->notifyJobs();
}

extern "C" void hsqml_set_debug_loglevel(int ll)
{
    Q_ASSERT (gManager);
    gManager->setLogLevel(ll);
}
