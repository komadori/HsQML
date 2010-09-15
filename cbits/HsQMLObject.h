#ifndef HSQML_OBJECT_H
#define HSQML_OBJECT_H

#include <QObject>

class HsQMLClass;

class HsQMLObject : public QObject
{
public:
  HsQMLObject(void*, HsQMLClass*);
  virtual ~HsQMLObject();
  virtual const QMetaObject* metaObject() const;
  virtual void* qt_metacast(const char*);
  virtual int qt_metacall(QMetaObject::Call, int, void**);
  void* haskell() const;

private:
  void* mHaskell;
  HsQMLClass* mKlass;
};
#endif /*HSQML_OBJECT_H*/
