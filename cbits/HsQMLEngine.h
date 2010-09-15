#ifndef HSQML_ENGINE_H
#define HSQML_ENGINE_H

#include <QSet>
#include <QString>
#include <QUrl>
#include <QDeclarativeEngine>

class HsQMLWindow;

struct HsQMLEngineConfig
{
  QObject* globalObject;
  QUrl initialURL;
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
