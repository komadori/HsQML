#ifndef HSQML_MANAGER_H
#define HSQML_MANAGER_H

#include <QApplication>
#include <QMutex>
#include <QVector>
#include <QUrl>

#include "hsqml.h"
#include "HsQMLEngine.h"

class HsQMLWindow;

class HsQMLManager : public QObject
{
    Q_OBJECT

public:
    HsQMLManager(int& argc, char** argv);
    ~HsQMLManager();
    void run();
    Q_SLOT void createEngine(HsQMLEngineConfig);

private:
    QApplication mApp;
    QMutex mMutex;
    QVector<HsQMLEngine*> mEngines;
};

extern QMutex gMutex;
extern HsQMLManager* gManager;
extern void HsQMLInitImpl();

static inline void HsQMLInit()
{
  if (!gManager) {
    HsQMLInitImpl();
  }
}

#endif /*HSQML_MANAGER_H*/
