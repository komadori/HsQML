#include <cstdlib>
#include <HsFFI.h>
#include <QtCore/QAtomicInt>
#include <QtCore/QMetaObject>
#include <QtCore/QMetaType>
#include <QtCore/QString>

#include "hsqml.h"
#include "HsQMLClass.h"
#include "HsQMLManager.h"

enum MDFields {
    MD_METHOD_COUNT   = 4,
    MD_PROPERTY_COUNT = 6,
};

QAtomicInt gClassId;

HsQMLClass::HsQMLClass(
    unsigned int*  metaData,
    char*          metaStrData,
    HsStablePtr    hsTypeRep,
    HsQMLUniformFunc* methods,
    HsQMLUniformFunc* properties)
    : mRefCount(0)
    , mMetaData(metaData)
    , mMetaStrData(metaStrData)
    , mHsTypeRep(hsTypeRep)
    , mMethodCount(metaData[MD_METHOD_COUNT])
    , mPropertyCount(metaData[MD_PROPERTY_COUNT])
    , mMethods(methods)
    , mProperties(properties)
{
    // Create meta-object
    QMetaObject tmp = {
          &QObject::staticMetaObject,
          mMetaStrData,
          mMetaData,
          0};
    mMetaObject = new QMetaObject(tmp);

    // Add reference
    ref(Handle);
}

HsQMLClass::~HsQMLClass()
{
    for (int i=0; i<mMethodCount; i++) {
        gManager->freeFun((HsFunPtr)mMethods[i]);
    }
    for (unsigned int i=0; i<2*mPropertyCount; i++) {
        if (mProperties[i]) {
            gManager->freeFun((HsFunPtr)mProperties[i]);
        }
    }
    gManager->freeStable(mHsTypeRep);
    std::free(mMetaData);
    std::free(mMetaStrData);
    std::free(mMethods);
    std::free(mProperties);
    delete mMetaObject;
}

const char* HsQMLClass::name()
{
    return mMetaObject->className();
} 

HsStablePtr HsQMLClass::hsTypeRep()
{
    return mHsTypeRep;
}

int HsQMLClass::methodCount()
{
    return mMethodCount;
}

int HsQMLClass::propertyCount()
{
    return mPropertyCount;
}

const HsQMLUniformFunc* HsQMLClass::methods()
{
    return mMethods;
}

const HsQMLUniformFunc* HsQMLClass::properties()
{
    return mProperties;
}

const QMetaObject* HsQMLClass::metaObj()
{
    return mMetaObject;
}

void HsQMLClass::ref(RefSrc src)
{
    int count = mRefCount.fetchAndAddOrdered(1);

    HSQML_LOG(count == 0 ? 1 : 2,
        QString().sprintf("%s Class, name=%s, src=%d, count=%d.",
        count ? "Ref" : "New", name(), src, count+1));
}

void HsQMLClass::deref(RefSrc src)
{
    int count = mRefCount.fetchAndAddOrdered(-1);

    HSQML_LOG(count == 1 ? 1 : 2,
        QString().sprintf("%s Class, name=%s, src=%d, count=%d.",
        count > 1 ? "Deref" : "Delete", name(), src, count));

    if (count == 1) {
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
    HsStablePtr    hsTypeRep,
    HsQMLUniformFunc* methods,
    HsQMLUniformFunc* properties)
{
    HsQMLClass* klass = new HsQMLClass(
            metaData, metaStrData, hsTypeRep, methods, properties);
    return (HsQMLClassHandle*)klass;
}

extern "C" void hsqml_finalise_class_handle(
    HsQMLClassHandle* hndl)
{
    HsQMLClass* klass = (HsQMLClass*)hndl;
    klass->deref(HsQMLClass::Handle);
}
