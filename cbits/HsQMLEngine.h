#ifndef HSQML_ENGINE_H
#define HSQML_ENGINE_H

#include <QtCore/QScopedPointer>
#include <QtCore/QString>
#include <QtCore/QUrl>
#include <QtScript/QScriptEngine>
#include <QtScript/QScriptValue>
#include <QtDeclarative/QDeclarativeEngine>
#include <QtDeclarative/QDeclarativeExpression>

#include "hsqml.h"

class HsQMLObjectProxy;
class HsQMLWindow;

struct HsQMLEngineConfig
{
    HsQMLEngineConfig()
        : contextObject(NULL)
        , showWindow(false)
        , setWindowTitle(false)
        , stopCb(NULL)
    {}

    HsQMLObjectProxy* contextObject;
    QString initialURL;
    bool showWindow;
    bool setWindowTitle;
    QString windowTitle;
    HsQMLTrivialCb stopCb;
};

class HsQMLScriptHack : public QObject
{
    Q_OBJECT

public:
    HsQMLScriptHack(QDeclarativeEngine*);
    Q_INVOKABLE virtual QObject* self();
    Q_INVOKABLE virtual void hack(QScriptValue);
    QScriptEngine* scriptEngine() const;

private:
    QScriptEngine* mEngine;
};

class HsQMLEngine : public QObject
{
    Q_OBJECT

public:
    HsQMLEngine(const HsQMLEngineConfig&);
    ~HsQMLEngine();
    virtual void childEvent(QChildEvent*);
    QDeclarativeEngine* declEngine();
    QScriptEngine* scriptEngine();

private:
    Q_DISABLE_COPY(HsQMLEngine)

    QDeclarativeEngine mDeclEngine;
    QScriptEngine* mScriptEngine;
    QScopedPointer<QObject> mContextObj;
    HsQMLTrivialCb mStopCb;
};

#endif /*HSQML_ENGINE_H*/
