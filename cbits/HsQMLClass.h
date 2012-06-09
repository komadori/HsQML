#ifndef HSQML_CLASS_H
#define HSQML_CLASS_H

#include <QObject>
#include <QAtomicInt>

#include "hsqml.h"

enum MDFields {
    MD_CLASS_NAME     = 1,
    MD_METHOD_COUNT   = 4,
    MD_PROPERTY_COUNT = 6,
};

class HsQMLClass
{
public:
    HsQMLClass(
        unsigned int*, char*, HsQMLUniformFunc*, HsQMLUniformFunc*);
    ~HsQMLClass();
    const HsQMLUniformFunc* methods();
    const HsQMLUniformFunc* properties();
    void ref();
    void deref();

private:
    QAtomicInt mRefCount;
    unsigned int* mMetaData;
    char* mMetaStrData;
    HsQMLUniformFunc* mMethods;
    HsQMLUniformFunc* mProperties;

public:
    const int mMethodCount;
    const int mPropertyCount;
    const QMetaObject mMetaObject;
};

#endif /*HSQML_CLASS_H*/
