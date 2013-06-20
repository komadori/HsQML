#ifndef HSQML_CLASS_H
#define HSQML_CLASS_H

#include <QtCore/QObject>
#include <QtCore/QAtomicInt>

#include "hsqml.h"

class HsQMLClass
{
public:
    HsQMLClass(
        unsigned int*, char*, HsQMLUniformFunc*, HsQMLUniformFunc*);
    ~HsQMLClass();
    const char* name();
    int methodCount();
    int propertyCount();
    const HsQMLUniformFunc* methods();
    const HsQMLUniformFunc* properties();
    const QMetaObject* metaObj();
    enum RefSrc {Handle, ObjProxy};
    void ref(RefSrc);
    void deref(RefSrc);

private:
    QAtomicInt mRefCount;
    unsigned int* mMetaData;
    char* mMetaStrData;
    int mMethodCount;
    int mPropertyCount;
    HsQMLUniformFunc* mMethods;
    HsQMLUniformFunc* mProperties;
    QMetaObject* mMetaObject;
};

#endif /*HSQML_CLASS_H*/
