#include <cstdlib>
#include <HsFFI.h>
#include <QAtomicInt>
#include <QMetaObject>
#include <QString>
#include <QDebug>

#include "hsqml.h"
#include "HsQMLClass.h"
#include "HsQMLManager.h"

QAtomicInt gClassId;

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
  for (int i=0; i<mMethodCount; i++) {
    gManager->freeFun((HsFunPtr)mMethods[i]);
  }
  for (unsigned int i=0; i<2*mPropertyCount; i++) {
    if (mProperties[i]) {
      gManager->freeFun((HsFunPtr)mProperties[i]);
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

extern "C" int hsqml_get_next_class_id()
{
  return gClassId.fetchAndAddRelaxed(1);
}

extern "C" HsQMLClassHandle* hsqml_create_class(
  unsigned int*  metaData,
  char*          metaStrData,
  HsQMLUniformFunc* methods,
  HsQMLUniformFunc* properties)
{
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
