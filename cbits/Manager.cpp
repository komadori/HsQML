#include <cstdio>
#include <cstdlib>
#include <QtCore/QBasicTimer>
#include <QtCore/QMetaType>
#include <QtCore/QMutexLocker>
#include <QtCore/QThread>
#ifdef Q_OS_MAC
#include <pthread.h>
#endif

#include "Canvas.h"
#include "Manager.h"
#include "Object.h"

// Declarations for part of Qt's internal API
Q_DECL_IMPORT const QVariant::Handler* qcoreVariantHandler();
namespace QVariantPrivate {
Q_DECL_IMPORT void registerHandler(
    const int name, const QVariant::Handler *handler);
}

static const char* cCounterNames[] = {
    "ClassCounter",
    "ObjectCounter",
    "QObjectCounter",
    "VariantCounter",
    "ClassSerial",
    "ObjectSerial"
};

static void dump_counters()
{
    Q_ASSERT (gManager);
    if (gManager->checkLogLevel(1)) {
        for (int i=0; i<HsQMLManager::TotalCounters; i++) {
            gManager->log(QString().sprintf("%s = %d.",
                cCounterNames[i], gManager->updateCounter(
                    static_cast<HsQMLManager::CounterId>(i), 0)));
        }
    }
}

static void hooked_construct(QVariant::Private* p, const void* copy)
{
    gManager->hookedConstruct(p, copy);
}

static void hooked_clear(QVariant::Private* p)
{
    gManager->hookedClear(p);
}

ManagerPointer gManager;

HsQMLManager::HsQMLManager(
    void (*freeFun)(HsFunPtr),
    void (*freeStable)(HsStablePtr))
    : mLogLevel(0)
    , mAtExit(false)
    , mFreeFun(freeFun)
    , mFreeStable(freeStable)
    , mOriginalHandler(qcoreVariantHandler())
    , mApp(NULL)
    , mLock(QMutex::Recursive)
    , mRunning(false)
    , mRunCount(0)
    , mShutdown(false)
    , mStackBase(NULL)
    , mStartCb(NULL)
    , mJobsCb(NULL)
    , mYieldCb(NULL)
    , mActiveEngine(NULL)
{
    // Get log level from environment
    const char* env = std::getenv("HSQML_DEBUG_LOG_LEVEL");
    if (env) {
        setLogLevel(QString(env).toInt());
    }
}

void HsQMLManager::setLogLevel(int ll)
{
    mLogLevel = ll;
    if (ll > 0 && !mAtExit) {
        if (atexit(&dump_counters) == 0) {
            mAtExit = true;
        }
        else {
            log("Failed to register callback with atexit().");
        }
    }
}

bool HsQMLManager::checkLogLevel(int ll)
{
    return mLogLevel >= ll;
}

void HsQMLManager::log(const QString& msg)
{
    QMutexLocker locker(&mLogLock);
    fprintf(stderr, "HsQML: %s\n", msg.toUtf8().data());
}

int HsQMLManager::updateCounter(CounterId id, int delta)
{
    return mCounters[id].fetchAndAddRelaxed(delta);
}

void HsQMLManager::freeFun(HsFunPtr funPtr)
{
    mFreeFun(funPtr);
}

void HsQMLManager::freeStable(HsStablePtr stablePtr)
{
    mFreeStable(stablePtr);
}

void HsQMLManager::registerObject(const QObject* obj)
{
    mObjectSet.insert(obj);
}

void HsQMLManager::unregisterObject(const QObject* obj)
{
    bool removed = mObjectSet.remove(obj);
    Q_ASSERT(removed);
}

void HsQMLManager::hookedConstruct(QVariant::Private* p, const void* copy)
{
    char guard;
    mOriginalHandler->construct(p, copy);
    void* pp = reinterpret_cast<void*>(p);
    // The QVariant internals sometimes use a special code path for pointer
    // values which avoids calling the handler functions. This makes it
    // difficult to use them to keep a reference count. However, it's my
    // observation that this only affects transient QVariants created on the
    // stack inside the JavaScript engine's marshalling code. The persistent
    // QVariants stored in the heap are manipulated using a more restricted set
    // of operations which always use the handler functions. Hence, by assuming
    // that the stack can be discounted, it's possible to keep an accurate
    // count of heap references using these hooks.
    if ((pp < &guard || pp > mStackBase) && p->type == QMetaType::QObjectStar) {
        if (isEventThread() && mObjectSet.contains(p->data.o)) {
            HsQMLObject* obj = static_cast<HsQMLObject*>(p->data.o);
            HsQMLObjectProxy* proxy = obj->proxy();
            proxy->ref(HsQMLObjectProxy::Variant);
            proxy->tryGCLock();
            updateCounter(VariantCount, 1);
        }
    }
}

void HsQMLManager::hookedClear(QVariant::Private* p)
{
    char guard;
    void* pp = reinterpret_cast<void*>(p);
    if ((pp < &guard || pp > mStackBase) && p->type == QMetaType::QObjectStar) {
        if (isEventThread() && mObjectSet.contains(p->data.o)) {
            HsQMLObject* obj = static_cast<HsQMLObject*>(p->data.o);
            obj->proxy()->deref(HsQMLObjectProxy::Variant);
            updateCounter(VariantCount, -1);
        }
    }
    mOriginalHandler->clear(p);
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

    // Check for invalid state
    if (mRunning) {
        return HSQML_EVLOOP_ALREADY_RUNNING;
    }
    else if (mShutdown) {
        return HSQML_EVLOOP_POST_SHUTDOWN;
    }

    // Check if event loop bound to a different thread
    if (mApp && !isEventThread()) {
        return HSQML_EVLOOP_WRONG_THREAD;
    }

#ifdef Q_OS_MAC
    if (!pthread_main_np()) {
        // Cocoa can only be run on the primordial thread and exec() doesn't
        // check this.
        return HSQML_EVLOOP_WRONG_THREAD;
    }
#endif

    // Check for non-threaded RTS
    if (yieldCb) {
        HSQML_LOG(0,
            "Warning: CPU cannot idle when using the non-threaded RTS.");
    }

    // Create application object
    if (!mApp) {
        mApp = new HsQMLManagerApp();
    }

    // Save stack base and callbacks
    mStackBase = &locker;
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
    int ret;
    do {
        ret = mApp->exec();

        // Kill all engines
        const QObjectList& cs = gManager->mApp->children();
        while (!cs.empty()) {
            delete cs.front();
        }

        // Cmd-Q on MacOS can kill the event loop before we're ready
        // Keep it running until a StopLoopEvent is received
    } while (ret == 0 && mRunning);

    // Remove redundant events
    QCoreApplication::removePostedEvents(
        mApp, HsQMLManagerApp::RemoveGCLockEvent);

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

void HsQMLManager::postObjectEvent(HsQMLObjectEvent* ev)
{
    QCoreApplication::postEvent(mApp, ev);
}

HsQMLManager::EventLoopStatus HsQMLManager::shutdown()
{
    QMutexLocker locker(&mLock);

    if (mRunning) {
        return HSQML_EVLOOP_ALREADY_RUNNING;
    }
    else if (isEventThread()) {
        HSQML_LOG(1, "Deleting QApplication object.");
        delete mApp;
        mApp = NULL;
        mShutdown = true;
    }
    else if (mApp) {
        return HSQML_EVLOOP_WRONG_THREAD; 
    }
    return HSQML_EVLOOP_OK;
}

HsQMLManagerApp::HsQMLManagerApp()
    : mArgC(1)
    , mArg0(0)
    , mArgV(&mArg0)
    , mHookedHandler(*gManager->mOriginalHandler)
    , mApp(mArgC, &mArgV)
{
    mApp.setQuitOnLastWindowClosed(false);

    // Install hooked handler for QVariants
    mHookedHandler.construct = &hooked_construct;
    mHookedHandler.clear = &hooked_clear;
    QVariantPrivate::registerHandler(0, &mHookedHandler);

    // Register custom types
    qRegisterMetaType<HsQMLEngineConfig>("HsQMLEngineConfig");
    qmlRegisterType<HsQMLCanvas>("HsQML.Canvas", 1, 0, "HaskellCanvas");
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
        gManager->mRunning = false;
        gManager->mApp->mApp.quit();
        break;}
    case HsQMLManagerApp::PendingJobsEvent: {
        gManager->mJobsCb();
        break;}
    case HsQMLManagerApp::RemoveGCLockEvent: {
        static_cast<HsQMLObjectEvent*>(ev)->process();
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

extern "C" HsQMLEventLoopStatus hsqml_evloop_shutdown()
{
    if (gManager) {
        return gManager->shutdown();
    }
    return HSQML_EVLOOP_OK;
}

extern "C" void hsqml_set_debug_loglevel(int ll)
{
    Q_ASSERT (gManager);
    gManager->setLogLevel(ll);
}
