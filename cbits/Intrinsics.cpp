#include <cstdlib>
#include <cstring>

#include <QtCore/QString>

#include "hsqml.h"

/* String */
extern "C" size_t hsqml_get_string_size()
{
    return sizeof(QString);
}

extern "C" void hsqml_init_string(HsQMLStringHandle* hndl)
{
    new((void*)hndl) QString();
}

extern "C" void hsqml_deinit_string(HsQMLStringHandle* hndl)
{
    QString* string = (QString*)hndl;
    string->~QString();
}

extern "C" UTF16* hsqml_marshal_string(
    int bufLen, HsQMLStringHandle* hndl)
{
    QString* string = (QString*)hndl;
    string->resize(bufLen);
    return reinterpret_cast<UTF16*>(string->data());
}

extern "C" int hsqml_unmarshal_string(
    HsQMLStringHandle* hndl, UTF16** bufPtr)
{
    QString* string = (QString*)hndl;
    *bufPtr = reinterpret_cast<UTF16*>(string->data());
    return string->length();
}
