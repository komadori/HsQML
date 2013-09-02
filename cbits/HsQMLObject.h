#ifndef HSQML_OBJECT_H
#define HSQML_OBJECT_H

#include <QtCore/QObject>
#include <QtCore/QAtomicInt>
#include <QtCore/QAtomicPointer>

class HsQMLEngine;
class HsQMLClass;
class HsQMLObject;

class HsQMLObjectProxy
{
public:
    HsQMLObjectProxy(HsStablePtr, HsQMLClass*);
    virtual ~HsQMLObjectProxy();
    HsStablePtr haskell() const;
    HsQMLClass* klass() const;
    HsQMLObject* object(HsQMLEngine*);
    void clearObject();
    HsQMLEngine* engine() const;
    enum RefSrc {Handle, Object};
    void ref(RefSrc);
    void deref(RefSrc);

private:
    HsStablePtr mHaskell;
    HsQMLClass* mKlass;
    QAtomicPointer<HsQMLObject> mObject;
    QAtomicInt mRefCount;
};

class HsQMLObject : public QObject
{
public:
    HsQMLObject(HsQMLObjectProxy*, HsQMLEngine*);
    virtual ~HsQMLObject();
    virtual const QMetaObject* metaObject() const;
    virtual void* qt_metacast(const char*);
    virtual int qt_metacall(QMetaObject::Call, int, void**);
    HsQMLObjectProxy* proxy() const;
    HsQMLEngine* engine() const;

private:
    HsQMLObjectProxy* mProxy;
    HsStablePtr mHaskell;
    HsQMLClass* mKlass;
    HsQMLEngine* mEngine;
};

#endif /*HSQML_OBJECT_H*/
