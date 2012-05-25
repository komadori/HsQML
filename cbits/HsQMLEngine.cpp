#include <QtDebug>

#include "hsqml.h"
#include "HsQMLManager.h"
#include "HsQMLEngine.h"
#include "HsQMLWindow.h"

HsQMLEngine::HsQMLEngine(HsQMLEngineConfig& config)
{
qDebug() << config.globalObject;
  mEngine.rootContext()->setContextObject(config.globalObject);

  HsQMLWindow* win = new HsQMLWindow(this);
  win->setSource(config.initialURL);
  win->setVisible(true);
  mWindows.insert(win);
}

HsQMLEngine::~HsQMLEngine()
{
}

QDeclarativeEngine* HsQMLEngine::engine()
{
  return &mEngine;
}

extern "C" void hsqml_create_engine(
  HsQMLObjectHandle* globalObject,
  const char* initialURL)
{
  HsQMLEngineConfig config;
  config.globalObject = (QObject*)globalObject;
  config.initialURL = QUrl(QString(initialURL));

  Q_ASSERT (gManager);
  QMetaObject::invokeMethod(
    gManager, "createEngine", Qt::QueuedConnection,
    Q_ARG(HsQMLEngineConfig, config));
}
