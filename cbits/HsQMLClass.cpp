#include <cstdlib>
#include <HsFFI.h>
#include <QMetaObject>
#include <QString>
#include <QMutex>
#include <QMap>
#include <QDebug>

#include "hsqml.h"
#include "HsQMLClass.h"

static QMutex gClassSetMutex;
static QSet<QString> gClassSet;

HsQMLClass::HsQMLClass(
  unsigned int*  metaData,
  char*          metaStrData,
  HsQMLUniformFunc* methods,
  HsQMLUniformFunc* properties)
  : mRefCount(1)
  , mMetaData(metaData)
  , mMetaStrData(metaStrData)
  , mMethods(methods)
  , mProperties(properties)
  , mMethodCount(metaData[MD_METHOD_COUNT])
  , mPropertyCount(metaData[MD_PROPERTY_COUNT])
  , mMetaObject(QMetaObject {{
      &QObject::staticMetaObject,
      mMetaStrData,
      mMetaData,
      0
    }})
{
  qDebug() << "class_created";
}

HsQMLClass::~HsQMLClass()
{
  QString className = QString(&mMetaStrData[mMetaData[1]]);
  gClassSetMutex.lock();
  gClassSet.remove(className);
  gClassSetMutex.unlock();
  for (int i=0; i<mMethodCount; i++) {
    hs_free_fun_ptr((HsFunPtr)mMethods[i]);
  }
  for (unsigned int i=0; i<2*mPropertyCount; i++) {
    if (mProperties[i]) {
      hs_free_fun_ptr((HsFunPtr)mProperties[i]);
    }
  }
  std::free(mMetaData);
  std::free(mMetaStrData);
  std::free(mMethods);
  std::free(mProperties);
  qDebug() << "class_destroyed";
}

const HsQMLUniformFunc* HsQMLClass::methods()
{
  return mMethods;
}

const HsQMLUniformFunc* HsQMLClass::properties()
{
  return mProperties;
}

void HsQMLClass::ref()
{
  mRefCount.ref();
}

void HsQMLClass::deref()
{
  if (!mRefCount.deref()) {
    delete this;
  }
}

extern "C" HsQMLClassHandle* hsqml_create_class(
  unsigned int*  metaData,
  char*          metaStrData,
  HsQMLUniformFunc* methods,
  HsQMLUniformFunc* properties)
{
  QString className = QString(&metaStrData[metaData[MD_CLASS_NAME]]);
  gClassSetMutex.lock();
  if (gClassSet.contains(className)) {
    gClassSetMutex.unlock();
    for (unsigned int i=0; i<metaData[MD_METHOD_COUNT]; i++) {
      hs_free_fun_ptr((HsFunPtr)methods[i]);
    }
    for (unsigned int i=0; i<2*metaData[MD_PROPERTY_COUNT]; i++) {
      if (properties[i]) {
        hs_free_fun_ptr((HsFunPtr)properties[i]);
      }
    }
    std::free(metaData);
    std::free(metaStrData);
    std::free(methods);
    std::free(properties);
    return NULL;
  }
  gClassSet.insert(className);
  gClassSetMutex.unlock();
  HsQMLClass* klass = new HsQMLClass(
    metaData, metaStrData, methods, properties);
  return (HsQMLClassHandle*)klass;
}

extern "C" void hsqml_finalise_class_handle(
  HsQMLClassHandle* hndl)
{
  HsQMLClass* klass = (HsQMLClass*)hndl;
  klass->deref();
}
