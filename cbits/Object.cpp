#include <HsFFI.h>
#include <QtCore/QString>
#include <QtCore/QMutexLocker>
#include <QtQml/QQmlEngine>

#include "Object.h"
#include "Class.h"
#include "Manager.h"

static const char* cRefSrcNames[] = {"Hndl", "Weak", "Obj", "Event", "Var"};

HsQMLObjectFinaliser::HsQMLObjectFinaliser(HsQMLObjFinaliserCb cb)
    : mFinaliseCb(cb)
{}

HsQMLObjectFinaliser::~HsQMLObjectFinaliser()
{
    gManager->freeFun(reinterpret_cast<HsFunPtr>(mFinaliseCb));
}

void HsQMLObjectFinaliser::finalise(HsQMLObjectProxy* proxy) const
{
    mFinaliseCb(reinterpret_cast<HsQMLObjectHandle*>(proxy));
}

HsQMLObjectProxy::HsQMLObjectProxy(HsStablePtr haskell, HsQMLClass* klass)
    : mHaskell(haskell)
    , mKlass(klass)
    , mSerial(gManager->updateCounter(HsQMLManager::ObjectSerial, 1))
    , mObject(NULL)
    , mRefCount(0)
{
    ref(Handle);
    mKlass->ref(HsQMLClass::ObjProxy);
    gManager->updateCounter(HsQMLManager::ObjectCount, 1);
}

HsQMLObjectProxy::~HsQMLObjectProxy()
{
    mKlass->deref(HsQMLClass::ObjProxy);
    gManager->updateCounter(HsQMLManager::ObjectCount, -1);
    gManager->freeStable(mHaskell);
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

        HSQML_LOG(5,
            QString().sprintf("New QObject, class=%s, id=%d, qptr=%p.",
            mKlass->name(), mSerial, mObject));
    }

    // Old objects may have lost their lock via weak references in addition
    // to new objects needing it.
    tryGCLock();

    return mObject;
}

void HsQMLObjectProxy::clearObject()
{
    Q_ASSERT(gManager->isEventThread());

    HSQML_LOG(5,
        QString().sprintf("Release QObject, class=%s, id=%d, qptr=%p.",
        mKlass->name(), mSerial, mObject));

    mObject = NULL;
    runFinalisers();
}

void HsQMLObjectProxy::tryGCLock()
{
    Q_ASSERT(gManager->isEventThread());

    if (mObject && mHndlCount.loadAcquire() > 0 && !mObject->isGCLocked()) {
        mObject->setGCLock();

        HSQML_LOG(5,
            QString().sprintf("Lock QObject, class=%s, id=%d, qptr=%p.",
            mKlass->name(), mSerial, mObject));
    }
}

void HsQMLObjectProxy::removeGCLock()
{
    Q_ASSERT(gManager->isEventThread());

    if (mObject && mHndlCount.loadAcquire() == 0) {
        if (mObject->isGCLocked()) {
            mObject->clearGCLock();

            HSQML_LOG(5,
                QString().sprintf("Unlock QObject, class=%s, id=%d, qptr=%p.",
                mKlass->name(), mSerial, mObject));
        }
        else {
            // If there had been a QML object then this would have happened
            // when the QML GC collected it.
            runFinalisers();
        }
    }
}

void HsQMLObjectProxy::addFinaliser(const HsQMLObjectFinaliser::Ref& f)
{
    QMutexLocker locker(&mFinaliseMutex);
    mFinalisers.append(f);
}

void HsQMLObjectProxy::runFinalisers()
{
    // Copy and clear finalisers under lock
    mFinaliseMutex.lock();
    const Finalisers fs = mFinalisers;
    mFinalisers.clear();
    mFinaliseMutex.unlock();

    // Call finalisers outside lock so they can re-addFinaliser()
    Q_FOREACH(const HsQMLObjectFinaliser::Ref& f, fs) {
        ref(Handle);
        f->finalise(this);
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
        QString().sprintf("%s ObjProxy, class=%s, id=%d, src=%s, count=%d.",
        count ? "Ref" : "New", mKlass->name(),
        mSerial, cRefSrcNames[src], count+1));

    if (src == Handle || src == Variant) {
        mHndlCount.fetchAndAddOrdered(1);
    }
}

void HsQMLObjectProxy::deref(RefSrc src)
{
    // Remove JavaScript GC lock when there are no strong handles
    if (src == Handle || src == Variant) {
        int hndlCount = mHndlCount.fetchAndAddOrdered(-1);
        if (hndlCount == 1 && mObject) {
            if (src == Handle) {
                // Handles can be dereferenced from any thread. The event will
                // remove the lock if there are still no handles by the time
                // it reaches the event loop.
                gManager->postObjectEvent(new HsQMLObjectEvent(this));
            }
            else {
                removeGCLock();
            }
        }
    }

    int count = mRefCount.fetchAndAddOrdered(-1);

    HSQML_LOG(count == 1 ? 3 : 4,
        QString().sprintf("%s ObjProxy, class=%s, id=%d, src=%s, count=%d.",
        count > 1 ? "Deref" : "Delete", mKlass->name(),
        mSerial, cRefSrcNames[src], count));

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
    QQmlEngine::setObjectOwnership(
        this, QQmlEngine::JavaScriptOwnership);
    mProxy->ref(HsQMLObjectProxy::Object);
    gManager->registerObject(this);
    gManager->updateCounter(HsQMLManager::QObjectCount, 1);
}

HsQMLObject::~HsQMLObject()
{
    mProxy->clearObject();
    mProxy->deref(HsQMLObjectProxy::Object);
    gManager->unregisterObject(this);
    gManager->updateCounter(HsQMLManager::QObjectCount, -1);
}

const QMetaObject* HsQMLObject::metaObject() const
{
    return QObject::d_ptr->metaObject ?
        QObject::d_ptr->dynamicMetaObject() : mKlass->metaObj();
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
    mGCLock = mEngine->declEngine()->newQObject(this);
}

void HsQMLObject::clearGCLock()
{
    mGCLock = QJSValue(QJSValue::NullValue);
}

bool HsQMLObject::isGCLocked() const
{
    return mGCLock.isQObject();
}

QJSValue* HsQMLObject::gcLockVar()
{
    return &mGCLock;
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

extern "C" int hsqml_object_set_active(
    HsQMLObjectHandle* hndl)
{
    HsQMLObjectProxy* proxy = (HsQMLObjectProxy*)hndl;
    if (proxy) {
        HsQMLEngine* engine = proxy->engine();
        if (engine) {
            gManager->setActiveEngine(engine);
        }
        else {
            // Nothing can be done if the object hasn't been marshalled before
            // because otherwise the relevant engine is unknown.
            return false;
        }
    }
    else {
        gManager->setActiveEngine(NULL);
    }
    return true;
}

extern "C" HsStablePtr hsqml_object_get_hs_typerep(
    HsQMLObjectHandle* hndl)
{
    HsQMLObjectProxy* proxy = (HsQMLObjectProxy*)hndl;
    return proxy->klass()->hsTypeRep();
}

extern HsStablePtr hsqml_object_get_hs_value(
    HsQMLObjectHandle* hndl)
{
    HsQMLObjectProxy* proxy = (HsQMLObjectProxy*)hndl;
    return proxy->haskell();
}

extern void* hsqml_object_get_pointer(
    HsQMLObjectHandle* hndl)
{
    HsQMLObjectProxy* proxy = reinterpret_cast<HsQMLObjectProxy*>(hndl);
    return (void*)proxy->object(gManager->activeEngine());
}

extern HsQMLJValHandle* hsqml_object_get_jval(
    HsQMLObjectHandle* hndl)
{
    HsQMLObjectProxy* proxy = reinterpret_cast<HsQMLObjectProxy*>(hndl);
    HsQMLObject* obj = proxy->object(gManager->activeEngine());
    return reinterpret_cast<HsQMLJValHandle*>(obj->gcLockVar());
}

extern HsQMLObjectHandle* hsqml_get_object_from_pointer(
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

extern HsQMLObjectHandle* hsqml_get_object_from_jval(
    HsQMLJValHandle* jvalHndl)
{
    QJSValue* jval = reinterpret_cast<QJSValue*>(jvalHndl);
    return hsqml_get_object_from_pointer(jval->toQObject());
}

extern void hsqml_object_reference_handle(
    HsQMLObjectHandle* hndl,
    int weak)
{
    HsQMLObjectProxy* proxy = (HsQMLObjectProxy*)hndl;
    proxy->ref(weak ? HsQMLObjectProxy::WeakHandle : HsQMLObjectProxy::Handle);
}

extern void hsqml_finalise_object_handle(
    HsQMLObjectHandle* hndl)
{
    if (hndl) {
        HsQMLObjectProxy* proxy = (HsQMLObjectProxy*)hndl;
        proxy->deref(HsQMLObjectProxy::Handle);
    }
}

extern void hsqml_finalise_object_weak_handle(
    HsQMLObjectHandle* hndl)
{
    if (hndl) {
        HsQMLObjectProxy* proxy = (HsQMLObjectProxy*)hndl;
        proxy->deref(HsQMLObjectProxy::WeakHandle);
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

extern HsQMLObjFinaliserHandle* hsqml_create_obj_finaliser(
    HsQMLObjFinaliserCb cb)
{
    return reinterpret_cast<HsQMLObjFinaliserHandle*>(
        new HsQMLObjectFinaliser::Ref(new HsQMLObjectFinaliser(cb)));
}

extern void hsqml_finalise_obj_finaliser(
    HsQMLObjFinaliserHandle* hndl)
{
    HsQMLObjectFinaliser::Ref* fp =
        reinterpret_cast<HsQMLObjectFinaliser::Ref*>(hndl);
    delete fp;
}

extern void hsqml_object_add_finaliser(
    HsQMLObjectHandle* hndl, HsQMLObjFinaliserHandle* fhndl)
{
    HsQMLObjectProxy* proxy = reinterpret_cast<HsQMLObjectProxy*>(hndl);
    proxy->addFinaliser(*reinterpret_cast<HsQMLObjectFinaliser::Ref*>(fhndl));
}
