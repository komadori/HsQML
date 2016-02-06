#ifndef HSQML_ENGINE_H
#define HSQML_ENGINE_H

#include <QtCore/QEvent>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QUrl>
#include <QtQml/QQmlEngine>
#include <QtQml/QQmlContext>
#include <QtQml/QQmlComponent>

#include "hsqml.h"

class HsQMLEngine;
class HsQMLObjectProxy;
class HsQMLWindow;

class HsQMLEngineProxy
{
public:
    HsQMLEngineProxy();
    ~HsQMLEngineProxy();

    void setEngine(HsQMLEngine*);
    HsQMLEngine* engine() const;
    void kill();
    bool dead() const;
    enum RefSrc {Handle, Engine, Event};
    void ref(RefSrc);
    void deref(RefSrc);

private:
    Q_DISABLE_COPY(HsQMLEngineProxy)

    HsQMLEngine* mEngine;
    bool mDead;
    int mSerial;
    QAtomicInt mRefCount;
};

class HsQMLEngineCreateEvent : public QEvent
{
public:
    HsQMLEngineCreateEvent(HsQMLEngineProxy*);
    HsQMLEngineProxy* proxy() const;
    virtual ~HsQMLEngineCreateEvent();

    // Config fields
    HsQMLObjectProxy* contextObject;
    QString initialURL;
    QStringList importPaths;
    QStringList pluginPaths;
    HsQMLTrivialCb stopCb;

private:
    HsQMLEngineProxy* mProxy;
};

class HsQMLEngine : public QObject
{
    Q_OBJECT

public:
    HsQMLEngine(const HsQMLEngineCreateEvent*, QObject* = NULL);
    ~HsQMLEngine();
    bool eventFilter(QObject*, QEvent*);
    QQmlEngine* declEngine();

private:
    Q_DISABLE_COPY(HsQMLEngine)

    Q_SLOT void componentStatus(QQmlComponent::Status);
    HsQMLEngineProxy* mProxy;
    QQmlEngine mEngine;
    QQmlComponent mComponent;
    QList<HsQMLObjectProxy*> mGlobals;
    QList<QObject*> mResources;
    HsQMLTrivialCb mStopCb;
};

#endif /*HSQML_ENGINE_H*/
