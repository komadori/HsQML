#include <HsFFI.h>
#include <QtCore/QString>
#include <QtDeclarative/QDeclarativeEngine>

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

HsQMLObject* HsQMLObjectProxy::object(HsQMLEngine* engine)
{
    Q_ASSERT(gManager->isEventThread());
    Q_ASSERT(engine);
    if (!mObject) {
        mObject = new HsQMLObject(this, engine);
        tryGCLock();

        HSQML_LOG(5,
            QString().sprintf("New QObject, class=%s, ptr=%p, proxy=%p.",
            mKlass->name(), mObject, this));
    }
    return mObject;
}

void HsQMLObjectProxy::clearObject()
{
    Q_ASSERT(gManager->isEventThread());

    mObject = NULL;

    HSQML_LOG(5,
        QString().sprintf("Release QObject, class=%s, ptr=%p, proxy=%p.",
        mKlass->name(), mObject, this));
}

void HsQMLObjectProxy::tryGCLock()
{
    Q_ASSERT(gManager->isEventThread());

    if (mObject && mHndlCount > 0 && !mObject->isGCLocked()) {
        mObject->setGCLock();

        HSQML_LOG(5,
            QString().sprintf("Lock QObject, class=%s, ptr=%p, proxy=%p.",
            mKlass->name(), mObject, this));
    }
}

void HsQMLObjectProxy::removeGCLock()
{
    Q_ASSERT(gManager->isEventThread());

    if (mObject && mHndlCount == 0 && mObject->isGCLocked()) {
        mObject->clearGCLock();

        HSQML_LOG(5,
            QString().sprintf("Unlock QObject, class=%s, ptr=%p, proxy=%p.",
            mKlass->name(), mObject, this));
    }
}

HsQMLEngine* HsQMLObjectProxy::engine() const
{
    if (mObject != NULL) {
        return mObject->engine();
    }
    return NULL;
}

void HsQMLObjectProxy::ref(RefSrc src)
{
    int count = mRefCount.fetchAndAddOrdered(1);

    HSQML_LOG(count == 0 ? 3 : 4,
        QString().sprintf("%s ObjProxy, class=%s, ptr=%p, src=%d, count=%d.",
        count ? "Ref" : "New", mKlass->name(), this, src, count+1));

    if (src == Handle) {
        mHndlCount.fetchAndAddOrdered(1);
    }
}

void HsQMLObjectProxy::deref(RefSrc src)
{
    // Remove JavaScript GC lock when there are no handles
    if (src == Handle) {
        int hndlCount = mHndlCount.fetchAndAddOrdered(-1);
        if (hndlCount == 1 && mObject) {
            // This will increment the reference count for the lifetime of the
            // of the event.
            gManager->postObjectEvent(new HsQMLObjectEvent(this));
        }
    }

    int count = mRefCount.fetchAndAddOrdered(-1);

    HSQML_LOG(count == 1 ? 3 : 4,
        QString().sprintf("%s ObjProxy, class=%s, ptr=%p, src=%d, count=%d.",
        count > 1 ? "Deref" : "Delete", mKlass->name(), this, src, count));

    if (count == 1) {
        delete this;
    }
}

HsQMLObjectEvent::HsQMLObjectEvent(HsQMLObjectProxy* proxy)
    : QEvent(HsQMLManagerApp::RemoveGCLockEvent)
    , mProxy(proxy)
{
    mProxy->ref(HsQMLObjectProxy::Event);
}

HsQMLObjectEvent::~HsQMLObjectEvent()
{
    mProxy->deref(HsQMLObjectProxy::Event);
}

void HsQMLObjectEvent::process()
{
    Q_ASSERT(type() == HsQMLManagerApp::RemoveGCLockEvent);
    mProxy->removeGCLock();
}

HsQMLObject::HsQMLObject(HsQMLObjectProxy* proxy, HsQMLEngine* engine)
    : mProxy(proxy)
    , mHaskell(proxy->haskell())
    , mKlass(proxy->klass())
    , mEngine(engine)
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
        QObject::d_ptr->metaObject : mKlass->metaObj();
}

void* HsQMLObject::qt_metacast(const char* clname)
{
    if (!clname) {
        return 0;
    }
    if (!strcmp(clname, mKlass->metaObj()->className())) {
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
    gManager->setActiveEngine(mEngine);
    if (QMetaObject::InvokeMetaMethod == c) {
        mKlass->methods()[id](this, a);
        id -= mKlass->methodCount();
    }
    else if (QMetaObject::ReadProperty == c) {
        mKlass->properties()[2*id](this, a);
        id -= mKlass->propertyCount();
    }
    else if (QMetaObject::WriteProperty == c) {
        HsQMLUniformFunc uf = mKlass->properties()[2*id+1];
        if (uf) {
            uf(this, a);
        }
        id -= mKlass->propertyCount();
    }
    else if (QMetaObject::QueryPropertyDesignable == c ||
             QMetaObject::QueryPropertyScriptable == c ||
             QMetaObject::QueryPropertyStored == c ||
             QMetaObject::QueryPropertyEditable == c ||
             QMetaObject::QueryPropertyUser == c) {
        id -= mKlass->propertyCount();
    }
    gManager->setActiveEngine(NULL);
    return id;
}

void HsQMLObject::setGCLock()
{
    mGCLock = mEngine->scriptEngine()->newQObject(
        this, QScriptEngine::ScriptOwnership);
}

void HsQMLObject::clearGCLock()
{
    mGCLock = QScriptValue();
}

bool HsQMLObject::isGCLocked() const
{
    return mGCLock.isValid();
}

HsQMLObjectProxy* HsQMLObject::proxy() const
{
    return mProxy;
}

HsQMLEngine* HsQMLObject::engine() const
{
    return mEngine;
}

extern "C" HsQMLObjectHandle* hsqml_create_object(
    HsStablePtr haskell, HsQMLClassHandle* kHndl)
{
    HsQMLObjectProxy* proxy = new HsQMLObjectProxy(haskell, (HsQMLClass*)kHndl);
    return (HsQMLObjectHandle*)proxy;
}

extern "C" void hsqml_object_set_active(
    HsQMLObjectHandle* hndl)
{
    HsQMLObjectProxy* proxy = (HsQMLObjectProxy*)hndl;
    if (proxy) {
        gManager->setActiveEngine(proxy->engine());
    }
    else {
        gManager->setActiveEngine(NULL);
    }
}

extern "C" HsStablePtr hsqml_object_get_hs_typerep(
    HsQMLObjectHandle* hndl)
{
    HsQMLObjectProxy* proxy = (HsQMLObjectProxy*)hndl;
    return proxy->klass()->hsTypeRep();
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
    return (void*)proxy->object(gManager->activeEngine());
}

extern HsQMLObjectHandle* hsqml_get_object_handle(
    void* ptr)
{
    // Return NULL if the input pointer is NULL
    if (!ptr) {
        return NULL;
    }

    // Get object proxy
    HsQMLObject* object = (HsQMLObject*)ptr;
    HsQMLObjectProxy* proxy = object->proxy();
    proxy->ref(HsQMLObjectProxy::Handle);
    proxy->tryGCLock();

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

extern void hsqml_fire_signal(
    HsQMLObjectHandle* hndl, int idx, void** args)
{
    HsQMLObjectProxy* proxy = (HsQMLObjectProxy*)hndl;
    HsQMLEngine* engine = proxy->engine();
    // Ignore objects which haven't been marshalled as they are not connected.
    if (engine) {
        // Clear active engine in case the slot code calls back into Haskell.
        Q_ASSERT(gManager->activeEngine() == engine);
        gManager->setActiveEngine(NULL);
        HsQMLObject* obj = proxy->object(engine);
        QMetaObject::activate(obj, proxy->klass()->metaObj(), idx, args);
    }
}
