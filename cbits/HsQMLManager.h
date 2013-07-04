#ifndef HSQML_MANAGER_H
#define HSQML_MANAGER_H

#include <QtCore/QAtomicPointer>
#include <QtCore/QReadWriteLock>
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
    int startEngine(const HsQMLEngineConfig&);

private:
    friend class HsQMLManagerApp;
    Q_DISABLE_COPY(HsQMLManager)

    int mLogLevel;
    void (*mFreeFun)(HsFunPtr);
    void (*mFreeStable)(HsStablePtr);
    HsQMLManagerApp* mApp;
    bool mAppRunning;
    QReadWriteLock mAppLock;
};

class HsQMLManagerApp : public QObject
{
    Q_OBJECT

public:
    HsQMLManagerApp();
    virtual ~HsQMLManagerApp();
    virtual void childEvent(QChildEvent*);
    virtual void customEvent(QEvent*);
    Q_SLOT void createEngine(HsQMLEngineConfig);
    int exec();

private:
    Q_DISABLE_COPY(HsQMLManagerApp)

    int mArgC;
    char mArg0;
    char* mArgV;
    QApplication mApp;
};

extern QAtomicPointer<HsQMLManager> gManager;

#endif /*HSQML_MANAGER_H*/
