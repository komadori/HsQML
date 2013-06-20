#include <cstdlib>
#include <HsFFI.h>
#include <QtCore/QAtomicInt>
#include <QtCore/QMetaObject>
#include <QtCore/QString>
#include <QtCore/QDebug>

#include "hsqml.h"
#include "HsQMLClass.h"
#include "HsQMLManager.h"

enum MDFields {
    MD_METHOD_COUNT   = 4,
    MD_PROPERTY_COUNT = 6,
};

QAtomicInt gClassId;

namespace QDeclarativePrivate
{
    enum RegistrationType {
        TypeRegistration = 0
    };

    int Q_DECLARATIVE_EXPORT qmlregister(RegistrationType, void*);
};

struct RegisterType {
    int version;

    int typeId;
    int listId;

    int objectSize;
    void (*create)(void*);

    QString noCreationReason;

    const char* uri;
    int versionMajor;
    int versionMinor;
    const char* elementName;
    const QMetaObject* metaObject;

    void* attachedPropertiesFunction;
    const QMetaObject* attachedPropertiesMetaObject;

    int parserStatusCast;
    int valueSourceCast;
    int valueInterceptorCast;

    QObject* (*extensionObjectCreate)(QObject *);
    const QMetaObject* extensionMetaObject;

    void* customParser;
    int revision;
};

static void* newObjectPtr(const void* pp)
{
    if (pp) {
        return new void*(*static_cast<void* const*>(pp));
    }
    else {
        return new void*();
    }
}

static void deleteObjectPtr(void* p)
{
    delete reinterpret_cast<void**>(p);
}

HsQMLClass::HsQMLClass(
    unsigned int*  metaData,
    char*          metaStrData,
    HsQMLUniformFunc* methods,
    HsQMLUniformFunc* properties)
    : mRefCount(0)
    , mMetaData(metaData)
    , mMetaStrData(metaStrData)
    , mMethods(methods)
    , mProperties(properties)
    , mMethodCount(metaData[MD_METHOD_COUNT])
    , mPropertyCount(metaData[MD_PROPERTY_COUNT])
{
    ref(Handle);

    // Create meta-object
    QMetaObject tmp = {
          &QObject::staticMetaObject,
          mMetaStrData,
          mMetaData,
          0};
    mMetaObject = new QMetaObject(tmp);

    // Register with Qt meta-type system
    QByteArray rawName(name());
    QByteArray pointerName(rawName + '*');
    int pointerType = QMetaType::registerType(
        pointerName, deleteObjectPtr, newObjectPtr);

    // Register with QML meta-type system
    RegisterType regInfo = {
        0,
        pointerType, 0,
        0, 0,
        QString(),
        0, 0, 0, 0, mMetaObject,
        0, 0,
        -1, -1, -1,
        0, 0,
        0,
        0
    };
    QDeclarativePrivate::qmlregister(
        QDeclarativePrivate::TypeRegistration, &regInfo);
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
    std::free(mMetaData);
    std::free(mMetaStrData);
    std::free(mMethods);
    std::free(mProperties);

    // Leak QMetaObject because there is no way to unregister it
    //delete mMetaObject;
}

const char* HsQMLClass::name()
{
    return mMetaObject->className();
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

    if (gLogLevel >= (count == 0 ? 1 : 2)) {
        qDebug() << QString().sprintf(
            "HsQML: %s Class, name=%s, src=%d, count=%d.",
            count ? "Ref" : "New", name(), src, count+1);
    }
}

void HsQMLClass::deref(RefSrc src)
{
    int count = mRefCount.fetchAndAddOrdered(-1);

    if (gLogLevel >= (count == 1 ? 1 : 2)) {
        qDebug() << QString().sprintf(
            "HsQML: %s Class, name=%s, src=%d, count=%d.",
            count > 1 ? "Deref" : "Delete", name(), src, count);
    }

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
    klass->deref(HsQMLClass::Handle);
}
