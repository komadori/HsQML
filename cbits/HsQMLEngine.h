#ifndef HSQML_ENGINE_H
#define HSQML_ENGINE_H

#include <QtCore/QScopedPointer>
#include <QtCore/QString>
#include <QtCore/QUrl>
#include <QtDeclarative/QDeclarativeEngine>

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
    QUrl initialURL;
    bool showWindow;
    bool setWindowTitle;
    QString windowTitle;
    HsQMLEngineStopCb stopCb;
};

class HsQMLEngine : public QObject
{
    Q_OBJECT

public:
    HsQMLEngine(const HsQMLEngineConfig&);
    ~HsQMLEngine();
    virtual void childEvent(QChildEvent*);
    QDeclarativeEngine* engine();

private:
    Q_DISABLE_COPY(HsQMLEngine)

    QDeclarativeEngine mEngine;
    QScopedPointer<QObject> mContextObj;
    HsQMLEngineStopCb mStopCb;
};

#endif /*HSQML_ENGINE_H*/
