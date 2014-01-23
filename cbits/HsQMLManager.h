#ifndef HSQML_MANAGER_H
#define HSQML_MANAGER_H

#include <QtCore/QAtomicPointer>
#include <QtCore/QAtomicInt>
#include <QtCore/QMutex>
#include <QtCore/QString>
#include <QtWidgets/QApplication>

#include "hsqml.h"
#include "HsQMLEngine.h"

#define HSQML_LOG(ll, msg) if (gManager->checkLogLevel(ll)) gManager->log(msg)

class HsQMLManagerApp;
class HsQMLObjectEvent;

class HsQMLManager
{
public:
    enum CounterId {
        ClassCount,
        ObjectCount,
        QObjectCount,
        ClassSerial,
        ObjectSerial,
        TotalCounters
    }; 

    HsQMLManager(
        void (*)(HsFunPtr),
        void (*)(HsStablePtr));
    void setLogLevel(int);
    bool checkLogLevel(int);
    void log(const QString&);
    int updateCounter(CounterId, int);
    void freeFun(HsFunPtr);
    void freeStable(HsStablePtr);
    bool isEventThread();
    typedef HsQMLEventLoopStatus EventLoopStatus;
    EventLoopStatus runEventLoop(
        HsQMLTrivialCb, HsQMLTrivialCb, HsQMLTrivialCb);
    EventLoopStatus requireEventLoop();
    void releaseEventLoop();
    void notifyJobs();
    void createEngine(const HsQMLEngineConfig&);
    void setActiveEngine(HsQMLEngine*);
    HsQMLEngine* activeEngine();
    void postObjectEvent(HsQMLObjectEvent*);

private:
    friend class HsQMLManagerApp;
    Q_DISABLE_COPY(HsQMLManager)

    int mLogLevel;
    QAtomicInt mCounters[TotalCounters];
    bool mAtExit;
    void (*mFreeFun)(HsFunPtr);
    void (*mFreeStable)(HsStablePtr);
    HsQMLManagerApp* mApp;
    QMutex mLock;
    bool mRunning;
    int mRunCount;
    HsQMLTrivialCb mStartCb;
    HsQMLTrivialCb mJobsCb;
    HsQMLTrivialCb mYieldCb;
    HsQMLEngine* mActiveEngine;
};

class HsQMLManagerApp : public QObject
{
    Q_OBJECT

public:
    HsQMLManagerApp();
    virtual ~HsQMLManagerApp();
    virtual void customEvent(QEvent*);
    virtual void timerEvent(QTimerEvent*);
    Q_SLOT void createEngine(HsQMLEngineConfig);
    int exec();

    enum CustomEventIndicies {
        StartedLoopEventIndex,
        StopLoopEventIndex,
        PendingJobsEventIndex,
        RemoveGCLockEventIndex
    };

    static const QEvent::Type StartedLoopEvent =
        static_cast<QEvent::Type>(QEvent::User+StartedLoopEventIndex);
    static const QEvent::Type StopLoopEvent =
        static_cast<QEvent::Type>(QEvent::User+StopLoopEventIndex);
    static const QEvent::Type PendingJobsEvent =
        static_cast<QEvent::Type>(QEvent::User+PendingJobsEventIndex);
    static const QEvent::Type RemoveGCLockEvent =
        static_cast<QEvent::Type>(QEvent::User+RemoveGCLockEventIndex);

private:
    Q_DISABLE_COPY(HsQMLManagerApp)

    int mArgC;
    char mArg0;
    char* mArgV;
    QApplication mApp;
};

class ManagerPointer : public QAtomicPointer<HsQMLManager>
{
public:
    HsQMLManager* operator->() const
    {
        return load();
    }

    operator HsQMLManager*() const
    {
        return load();
    }
};

extern ManagerPointer gManager;

#endif /*HSQML_MANAGER_H*/
