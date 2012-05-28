#include <QMetaType>

#include "HsQMLManager.h"

QMutex gMutex;
HsQMLManager* gManager;

HsQMLManager::HsQMLManager(
  int& argc,
  char** argv,
  void (*freeFun)(HsFunPtr),
  void (*freeStable)(HsStablePtr))
  : mApp(argc, argv)
  , mFreeFun(freeFun)
  , mFreeStable(freeStable)
{
}

HsQMLManager::~HsQMLManager()
{
}

void HsQMLManager::run()
{
    mApp.exec();

    // Delete engines once the event loop returns
    for (QVector<HsQMLEngine*>::iterator it = mEngines.begin();
         it != mEngines.end(); ++it) {
        delete *it;
    }
    mEngines.clear();
}

void HsQMLManager::freeFun(HsFunPtr funPtr)
{
    mFreeFun(funPtr);
}

void HsQMLManager::freeStable(HsStablePtr stablePtr)
{
    mFreeStable(stablePtr);
}

void HsQMLManager::createEngine(HsQMLEngineConfig config)
{
  mEngines.push_back(new HsQMLEngine(config));
}

extern "C" void hsqml_init(
  void (*free_fun)(HsFunPtr),
  void (*free_stable)(HsStablePtr))
{
  gMutex.lock();
  if (!gManager) {
    qRegisterMetaType<HsQMLEngineConfig>("HsQMLEngineConfig");

    int* argcp = new int[1];
    *argcp = 1;
    char** argv = new char*[1];
    argv[0] = new char[1];
    argv[0][0] = '\0';
    gManager = new HsQMLManager(*argcp, argv, free_fun, free_stable);
  }
  gMutex.unlock();
}

extern "C" void hsqml_run()
{
  Q_ASSERT (gManager);
  gManager->run();
}
