#include <cstdlib>
#include <cstring>
#include <HsFFI.h>
#include <QtCore/QMetaObject>
#include <QtCore/QMetaType>
#include <QtCore/QString>

#include "hsqml.h"
#include "Class.h"
#include "Manager.h"

enum MDFields {
    MD_METHOD_COUNT   = 4,
    MD_PROPERTY_COUNT = 6,
};

static const char* cRefSrcNames[] = {"Hndl", "Proxy"};

HsQMLClass::HsQMLClass(
    unsigned int*  metaData,
    unsigned int*  metaStrInfo,
    char*          metaStrChar,
    HsStablePtr    hsTypeRep,
    HsQMLUniformFunc* methods,
    HsQMLUniformFunc* properties)
    : mRefCount(0)
    , mMetaData(metaData)
    , mHsTypeRep(hsTypeRep)
    , mMethodCount(metaData[MD_METHOD_COUNT])
    , mPropertyCount(metaData[MD_PROPERTY_COUNT])
    , mMethods(methods)
    , mProperties(properties)
{
    // Create string data
    unsigned int strCount = metaStrInfo[0];
    unsigned int strLength = metaStrInfo[strCount];
    size_t arrayOff = strCount*sizeof(QByteArrayData);
    size_t arraySize = arrayOff+strLength;
    mMetaStrData.reset(new char[arraySize]);
    for (int i=0; i<strCount; i++) {
        int start = i > 0 ? metaStrInfo[i] : 0;
        int size = metaStrInfo[i+1] - start;
        int offset = arrayOff-(i*sizeof(QByteArrayData))+start;
        QByteArrayData data = {
            Q_REFCOUNT_INITIALIZE_STATIC, size-1, 0, 0, offset};
        new(&mMetaStrData[i*sizeof(QByteArrayData)]) QByteArrayData(data);
    }
    std::memcpy(&mMetaStrData[arrayOff], metaStrChar, strLength);

    // Create meta-object
    QMetaObject metaObj = {
          &QObject::staticMetaObject,
          reinterpret_cast<QByteArrayData*>(mMetaStrData.data()),
          mMetaData,
          0,
          0};
    mMetaObject = metaObj;

    // Add reference
    ref(Handle);

    gManager->updateCounter(HsQMLManager::ClassCount, 1);
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
    std::free(mMethods);
    std::free(mProperties);

    gManager->updateCounter(HsQMLManager::ClassCount, -1);
}

const char* HsQMLClass::name()
{
    return mMetaObject.className();
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
    return &mMetaObject;
}

void HsQMLClass::ref(RefSrc src)
{
    int count = mRefCount.fetchAndAddOrdered(1);

    HSQML_LOG(count == 0 ? 1 : 2,
        QString().sprintf("%s Class, name=%s, src=%s, count=%d.",
        count ? "Ref" : "New", name(), cRefSrcNames[src], count+1));
}

void HsQMLClass::deref(RefSrc src)
{
    int count = mRefCount.fetchAndAddOrdered(-1);

    HSQML_LOG(count == 1 ? 1 : 2,
        QString().sprintf("%s Class, name=%s, src=%s, count=%d.",
        count > 1 ? "Deref" : "Delete", name(), cRefSrcNames[src], count));

    if (count == 1) {
        delete this;
    }
}

extern "C" int hsqml_get_next_class_id()
{
    return gManager->updateCounter(HsQMLManager::ClassSerial, 1);
}

extern "C" HsQMLClassHandle* hsqml_create_class(
    unsigned int*  metaData,
    unsigned int*  metaStrInfo,
    char*          metaStrChar,
    HsStablePtr    hsTypeRep,
    HsQMLUniformFunc* methods,
    HsQMLUniformFunc* properties)
{
    HsQMLClass* klass = new HsQMLClass(
        metaData, metaStrInfo, metaStrChar, hsTypeRep, methods, properties);
    return (HsQMLClassHandle*)klass;
}

extern "C" void hsqml_finalise_class_handle(
    HsQMLClassHandle* hndl)
{
    HsQMLClass* klass = (HsQMLClass*)hndl;
    klass->deref(HsQMLClass::Handle);
}
