#ifndef HSQML_OBJECT_H
#define HSQML_OBJECT_H

#include <QtCore/QObject>
#include <QtCore/QAtomicInt>
#include <QtCore/QEvent>
#include <QtCore/QExplicitlySharedDataPointer>
#include <QtCore/QMutex>
#include <QtCore/QVarLengthArray>
#include <QtQml/QJSValue>

#include "hsqml.h"

class HsQMLEngine;
class HsQMLClass;
class HsQMLObject;
class HsQMLObjectProxy;

class HsQMLObjectFinaliser : public QSharedData
{
public:
    typedef QExplicitlySharedDataPointer<HsQMLObjectFinaliser> Ref;

    HsQMLObjectFinaliser(HsQMLObjFinaliserCb);
    ~HsQMLObjectFinaliser();
    void finalise(HsQMLObjectProxy*) const;

private:
    Q_DISABLE_COPY(HsQMLObjectFinaliser);

    HsQMLObjFinaliserCb mFinaliseCb;
};

class HsQMLObjectProxy
{
public:
    HsQMLObjectProxy(HsStablePtr, HsQMLClass*);
    virtual ~HsQMLObjectProxy();
    HsStablePtr haskell() const;
    HsQMLClass* klass() const;
    HsQMLObject* object(HsQMLEngine*);
    void clearObject();
    void tryGCLock();
    void removeGCLock();
    void addFinaliser(const HsQMLObjectFinaliser::Ref&);
    void runFinalisers();
    HsQMLEngine* engine() const;
    enum RefSrc {Handle, WeakHandle, Object, Event, Variant};
    void ref(RefSrc);
    void deref(RefSrc);

private:
    Q_DISABLE_COPY(HsQMLObjectProxy);

    HsStablePtr mHaskell;
    HsQMLClass* mKlass;
    int mSerial;
    HsQMLObject* volatile mObject;
    QAtomicInt mRefCount;
    QAtomicInt mHndlCount;
    QMutex mFinaliseMutex;
    typedef QVarLengthArray<HsQMLObjectFinaliser::Ref, 1> Finalisers;
    Finalisers mFinalisers;
};

class HsQMLObjectEvent : public QEvent
{
public:
    HsQMLObjectEvent(HsQMLObjectProxy*);
    virtual ~HsQMLObjectEvent();
    void process();

private:
    HsQMLObjectProxy* mProxy;
};

class HsQMLObject : public QObject
{
public:
    HsQMLObject(HsQMLObjectProxy*, HsQMLEngine*);
    virtual ~HsQMLObject();
    virtual const QMetaObject* metaObject() const;
    virtual void* qt_metacast(const char*);
    virtual int qt_metacall(QMetaObject::Call, int, void**);
    void setGCLock();
    void clearGCLock();
    bool isGCLocked() const;
    QJSValue* gcLockVar();
    HsQMLObjectProxy* proxy() const;
    HsQMLEngine* engine() const;

private:
    Q_DISABLE_COPY(HsQMLObject);

    HsQMLObjectProxy* mProxy;
    HsStablePtr mHaskell;
    HsQMLClass* mKlass;
    HsQMLEngine* mEngine;
    QJSValue mGCLock;
};

#endif /*HSQML_OBJECT_H*/
