#include <QMetaType>

#include "HsQMLManager.h"

QMutex gMutex;
HsQMLManager* gManager;

void HsQMLInitImpl()
{
  gMutex.lock();
  if (!gManager) {
    qRegisterMetaType<HsQMLEngineConfig>("HsQMLEngineConfig");

    int* argcp = new int[1];
    *argcp = 1;
    char** argv = new char*[1];
    argv[0] = new char[1];
    argv[0][0] = '\0';
    gManager = new HsQMLManager(*argcp, argv);
  }
  gMutex.unlock();
}

HsQMLManager::HsQMLManager(int& argc, char** argv)
  : mApp(argc, argv)
{
}

HsQMLManager::~HsQMLManager()
{
}

void HsQMLManager::run()
{
    mApp.exec();
}

void HsQMLManager::createEngine(HsQMLEngineConfig config)
{
  mEngines.push_back(new HsQMLEngine(config));
}

extern "C" void hsqml_run()
{
  HsQMLInit();
  gManager->run();
}
