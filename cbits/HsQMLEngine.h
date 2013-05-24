#ifndef HSQML_ENGINE_H
#define HSQML_ENGINE_H

#include <QtCore/QSet>
#include <QtCore/QString>
#include <QtCore/QUrl>
#include <QtDeclarative/QDeclarativeEngine>

class HsQMLObjectProxy;
class HsQMLWindow;

struct HsQMLEngineConfig
{
    HsQMLEngineConfig()
        : contextObject(NULL)
        , showWindow(false)
        , setWindowTitle(false)
    {}

    HsQMLObjectProxy* contextObject;
    QUrl initialURL;
    bool showWindow;
    bool setWindowTitle;
    QString windowTitle;
};

class HsQMLEngine : public QObject
{
    Q_OBJECT

public:
    HsQMLEngine(HsQMLEngineConfig&);
    ~HsQMLEngine();
    QDeclarativeEngine* engine();

private:
    QDeclarativeEngine mEngine;
    QSet<HsQMLWindow*> mWindows;
};

#endif /*HSQML_ENGINE_H*/
