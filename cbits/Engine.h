#ifndef HSQML_ENGINE_H
#define HSQML_ENGINE_H

#include <QtCore/QScopedPointer>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QUrl>
#include <QtQml/QQmlEngine>
#include <QtQml/QQmlContext>
#include <QtQml/QQmlComponent>

#include "hsqml.h"

class HsQMLObjectProxy;
class HsQMLWindow;

struct HsQMLEngineConfig
{
    HsQMLEngineConfig()
        : contextObject(NULL)
        , stopCb(NULL)
    {}

    HsQMLObjectProxy* contextObject;
    QString initialURL;
    QStringList importPaths;
    QStringList pluginPaths;
    HsQMLTrivialCb stopCb;
};

class HsQMLEngine : public QObject
{
    Q_OBJECT

public:
    HsQMLEngine(const HsQMLEngineConfig&);
    ~HsQMLEngine();
    bool eventFilter(QObject*, QEvent*);
    QQmlEngine* declEngine();

private:
    Q_DISABLE_COPY(HsQMLEngine)

    Q_SLOT void componentStatus(QQmlComponent::Status);
    QQmlEngine mEngine;
    QQmlComponent mComponent;
    QList<QObject*> mObjects;
    HsQMLTrivialCb mStopCb;
};

#endif /*HSQML_ENGINE_H*/
