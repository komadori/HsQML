#include <HsFFI.h>
#include <QDeclarativeEngine>

#include "HsQMLObject.h"
#include "HsQMLClass.h"

HsQMLObject::HsQMLObject(void* haskell, HsQMLClass* klass)
  : mHaskell(haskell)
  , mKlass(klass)
{
  mKlass->ref();
}

HsQMLObject::~HsQMLObject()
{
  hs_free_stable_ptr((HsStablePtr)mHaskell);
  mKlass->deref();
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

void* HsQMLObject::haskell() const
{
  return mHaskell;
}

extern "C" HsQMLObjectHandle* hsqml_create_object(
  void* haskell, HsQMLClassHandle* kHndl)
{
  HsQMLObject* obj = new HsQMLObject(haskell, (HsQMLClass*)kHndl);
  QDeclarativeEngine::setObjectOwnership(
    obj, QDeclarativeEngine::JavaScriptOwnership);
  return (HsQMLObjectHandle*)obj;
}

extern void* hsqml_get_haskell(
  HsQMLObjectHandle* hndl)
{
  HsQMLObject* obj = (HsQMLObject*)hndl;
  return  obj->haskell();
}

