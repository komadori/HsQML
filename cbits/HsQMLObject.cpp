#include <HsFFI.h>
#include <QDeclarativeEngine>
#include <QString>
#include <QDebug>

#include "HsQMLObject.h"
#include "HsQMLClass.h"
#include "HsQMLManager.h"

HsQMLObjectProxy::HsQMLObjectProxy(HsStablePtr haskell, HsQMLClass* klass)
    : mHaskell(haskell)
    , mKlass(klass)
    , mObject(NULL)
    , mRefCount(0)
{
    ref(Handle);
    mKlass->ref(HsQMLClass::ObjProxy);
}

HsQMLObjectProxy::~HsQMLObjectProxy()
{
    mKlass->deref(HsQMLClass::ObjProxy);
}

HsStablePtr HsQMLObjectProxy::haskell() const
{
    return mHaskell;
}

HsQMLClass* HsQMLObjectProxy::klass() const
{
    return mKlass;
}

HsQMLObject* HsQMLObjectProxy::object()
{
    if (!mObject) {
        mObject = new HsQMLObject(this);
    }
    return mObject;
}

void HsQMLObjectProxy::clearObject()
{
    mObject = NULL;
}

void HsQMLObjectProxy::ref(RefSrc src)
{
    int count = mRefCount.fetchAndAddOrdered(1);

    if (gLogLevel >= (count == 0 ? 3 : 4)) {
        qDebug() << QString().sprintf(
            "HsQML: %s ObjProxy, class=%s, ptr=%p, src=%d, count=%d.",
            count ? "Ref" : "New", mKlass->name(), this, src, count+1);
    }
}

void HsQMLObjectProxy::deref(RefSrc src)
{
    int count = mRefCount.fetchAndAddOrdered(-1);

    if (gLogLevel >= (count == 1 ? 3 : 4)) {
        qDebug() << QString().sprintf(
            "HsQML: %s ObjProxy, class=%s, ptr=%p, src=%d, count=%d.",
            count > 1 ? "Deref" : "Delete", mKlass->name(), this, src, count);
    }

    if (count == 1) {
        delete this;
    }
}

HsQMLObject::HsQMLObject(HsQMLObjectProxy* proxy)
    : mProxy(proxy)
    , mHaskell(proxy->haskell())
    , mKlass(proxy->klass())
{
    QDeclarativeEngine::setObjectOwnership(
        this, QDeclarativeEngine::JavaScriptOwnership);
    mProxy->ref(HsQMLObjectProxy::Object);
}

HsQMLObject::~HsQMLObject()
{
    mProxy->clearObject();
    mProxy->deref(HsQMLObjectProxy::Object);
}

const QMetaObject* HsQMLObject::metaObject() const
{
    return QObject::d_ptr->metaObject ?
        QObject::d_ptr->metaObject : &mKlass->mMetaObject;
}

void* HsQMLObject::qt_metacast(const char* clname)
{
    if (!clname) {
        return 0;
    }
    if (!strcmp(clname,
            mKlass->mMetaObject.d.stringdata +
            mKlass->mMetaObject.d.data[MD_CLASS_NAME])) {
        return static_cast<void*>(const_cast<HsQMLObject*>(this));
    }
    return QObject::qt_metacast(clname);
}

int HsQMLObject::qt_metacall(QMetaObject::Call c, int id, void** a)
{
    id = QObject::qt_metacall(c, id, a);
    if (id < 0) {
        return id;
    }
    if (QMetaObject::InvokeMetaMethod == c) {
        mKlass->methods()[id](this, a);
        id -= mKlass->mMethodCount;
    }
    else if (QMetaObject::ReadProperty == c) {
        mKlass->properties()[2*id](this, a);
        id -= mKlass->mPropertyCount;
    }
    else if (QMetaObject::WriteProperty == c) {
        HsQMLUniformFunc uf = mKlass->properties()[2*id+1];
        if (uf) {
            uf(this, a);
        }
        id -= mKlass->mPropertyCount;
    }
    else if (QMetaObject::QueryPropertyDesignable == c ||
             QMetaObject::QueryPropertyScriptable == c ||
             QMetaObject::QueryPropertyStored == c ||
             QMetaObject::QueryPropertyEditable == c ||
             QMetaObject::QueryPropertyUser == c) {
        id -= mKlass->mPropertyCount;
    }
    return id;
}

HsQMLObjectProxy* HsQMLObject::proxy() const
{
    return mProxy;
}

extern "C" HsQMLObjectHandle* hsqml_create_object(
    HsStablePtr haskell, HsQMLClassHandle* kHndl)
{
    HsQMLObjectProxy* proxy = new HsQMLObjectProxy(haskell, (HsQMLClass*)kHndl);
    return (HsQMLObjectHandle*)proxy;
}

extern HsStablePtr hsqml_object_get_haskell(
    HsQMLObjectHandle* hndl)
{
    HsQMLObjectProxy* proxy = (HsQMLObjectProxy*)hndl;
    return proxy->haskell();
}

extern void* hsqml_object_get_pointer(
    HsQMLObjectHandle* hndl)
{
    HsQMLObjectProxy* proxy = (HsQMLObjectProxy*)hndl;
    return (void*)proxy->object();
}

extern HsQMLObjectHandle* hsqml_get_object_handle(
    void* ptr, HsQMLClassHandle* klassHndl)
{
    // Return NULL if the input pointer is NULL
    if (!ptr) {
        return NULL;
    }

    // Return NULL if the object type doesn't match any supplied class
    if (klassHndl) {
        HsQMLClass* klass = (HsQMLClass*)klassHndl;
        QObject* qObj = static_cast<QObject*>(ptr);
        if (0 != strcmp(
            qObj->metaObject()->className(),
            klass->mMetaObject.className())) {
            return NULL;
        }
    }

    // Return object proxy
    HsQMLObject* object = (HsQMLObject*)ptr;
    HsQMLObjectProxy* proxy = object->proxy();
    proxy->ref(HsQMLObjectProxy::Handle);
    return (HsQMLObjectHandle*)proxy;
}

extern void hsqml_finalise_object_handle(
    HsQMLObjectHandle* hndl)
{
    if (hndl) {
        HsQMLObjectProxy* proxy = (HsQMLObjectProxy*)hndl;
        proxy->deref(HsQMLObjectProxy::Handle);
    }
}
