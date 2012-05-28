#include <QtDebug>

#include "hsqml.h"
#include "HsQMLManager.h"
#include "HsQMLEngine.h"
#include "HsQMLObject.h"
#include "HsQMLWindow.h"

HsQMLEngine::HsQMLEngine(HsQMLEngineConfig& config)
{
  // Obtain, re-parent, and set QML global object
  QObject* globalObject = config.globalObject->object();
  globalObject->setParent(this);
  mEngine.rootContext()->setContextObject(globalObject);

  // Create window
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
  config.globalObject = (HsQMLObjectProxy*)globalObject;
  config.initialURL = QUrl(QString(initialURL));

  Q_ASSERT (gManager);
  QMetaObject::invokeMethod(
    gManager, "createEngine", Qt::QueuedConnection,
    Q_ARG(HsQMLEngineConfig, config));
}
