#ifndef HSQML_MANAGER_H
#define HSQML_MANAGER_H

#include <QtCore/QAtomicPointer>
#include <QtCore/QMutex>
#include <QtCore/QMutexLocker>
#include <QtCore/QString>
#include <QtGui/QApplication>

#include "hsqml.h"
#include "HsQMLEngine.h"

#define HSQML_LOG(ll, msg) if (gManager->checkLogLevel(ll)) gManager->log(msg)

class HsQMLManagerApp;

class HsQMLManager
{
public:
    HsQMLManager(
        void (*)(HsFunPtr),
        void (*)(HsStablePtr));
    void setLogLevel(int);
    bool checkLogLevel(int);
    void log(const QString&);
    void freeFun(HsFunPtr);
    void freeStable(HsStablePtr);
    typedef HsQMLEventLoopStatus EventLoopStatus;
    EventLoopStatus runEventLoop(HsQMLTrivialCb, HsQMLTrivialCb);
    EventLoopStatus requireEventLoop();
    void releaseEventLoop();
    void createEngine(const HsQMLEngineConfig&);

private:
    friend class HsQMLManagerApp;
    Q_DISABLE_COPY(HsQMLManager)

    int mLogLevel;
    void (*mFreeFun)(HsFunPtr);
    void (*mFreeStable)(HsStablePtr);
    HsQMLManagerApp* mApp;
    QMutex mLock;
    bool mRunning;
    int mRunCount;
    HsQMLTrivialCb mStartCb;
    HsQMLTrivialCb mYieldCb;
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
        StopLoopEventIndex
    };

    static const QEvent::Type StartedLoopEvent =
        static_cast<QEvent::Type>(QEvent::User+StartedLoopEventIndex);
    static const QEvent::Type StopLoopEvent =
        static_cast<QEvent::Type>(QEvent::User+StopLoopEventIndex);

private:
    Q_DISABLE_COPY(HsQMLManagerApp)

    int mArgC;
    char mArg0;
    char* mArgV;
    QApplication mApp;
};

extern QAtomicPointer<HsQMLManager> gManager;

#endif /*HSQML_MANAGER_H*/
