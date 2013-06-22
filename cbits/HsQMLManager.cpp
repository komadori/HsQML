#include <cstdlib>
#include <QtCore/QMetaType>

#include "HsQMLManager.h"

QMutex gMutex;
HsQMLManager* gManager;
bool gLogLevelSet;
int gLogLevel;

HsQMLManager::HsQMLManager(
    int& argc,
    char** argv,
    void (*freeFun)(HsFunPtr),
    void (*freeStable)(HsStablePtr))
    : mApp(argc, argv)
    , mFreeFun(freeFun)
    , mFreeStable(freeStable)
{
    mApp.setQuitOnLastWindowClosed(false);
}

HsQMLManager::~HsQMLManager()
{
}

void HsQMLManager::childEvent(QChildEvent* ev)
{
    if (ev->removed() && children().size() == 0) {
        mApp.quit();
    }
}

void HsQMLManager::run()
{
    mApp.exec();
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
    HsQMLEngine* engine = new HsQMLEngine(config);
    engine->setParent(this);
}

extern "C" void hsqml_init(
    void (*free_fun)(HsFunPtr),
    void (*free_stable)(HsStablePtr))
{
    gMutex.lock();
    if (!gManager) {
        qRegisterMetaType<HsQMLEngineConfig>("HsQMLEngineConfig");

        if (!gLogLevelSet) {
            const char* env = std::getenv("HSQML_DEBUG_LOG_LEVEL");
            if (env) {
                gLogLevel = QString(env).toInt();
            }
        }

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

extern "C" void hsqml_set_debug_loglevel(int level)
{
    gLogLevelSet = true;
    gLogLevel = level;
}
