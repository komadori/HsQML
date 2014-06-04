#include <QtCore/QString>
#include <QtCore/QMetaType>
#include <QtQml/QJSValue>

#include "Manager.h"

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
    QString* string = reinterpret_cast<QString*>(hndl);
    string->~QString();
}

extern "C" UTF16* hsqml_write_string(
    int bufLen, HsQMLStringHandle* hndl)
{
    QString* string = reinterpret_cast<QString*>(hndl);
    string->resize(bufLen);
    return reinterpret_cast<UTF16*>(string->data());
}

extern "C" int hsqml_read_string(
    HsQMLStringHandle* hndl, UTF16** bufPtr)
{
    QString* string = reinterpret_cast<QString*>(hndl);
    *bufPtr = reinterpret_cast<UTF16*>(string->data());
    return string->length();
}

/* JSValue */
extern "C" size_t hsqml_get_jval_size()
{
    return sizeof(QJSValue);
}

extern "C" int hsqml_get_jval_typeid()
{
    return qMetaTypeId<QJSValue>();
}

extern "C" void hsqml_init_jval_null(HsQMLJValHandle* hndl, int undef)
{
    new((void*)hndl) QJSValue(undef ?
        QJSValue::UndefinedValue : QJSValue::NullValue);
}

extern "C" void hsqml_set_jval(HsQMLJValHandle* hndl, HsQMLJValHandle* srch)
{
    QJSValue* dstValue = reinterpret_cast<QJSValue*>(hndl);
    QJSValue* srcValue = reinterpret_cast<QJSValue*>(srch);
    *dstValue = *srcValue;
}

extern "C" void hsqml_deinit_jval(HsQMLJValHandle* hndl)
{
    QJSValue* value = reinterpret_cast<QJSValue*>(hndl);
    value->~QJSValue();
}


extern "C" void hsqml_init_jval_bool(HsQMLJValHandle* hndl, int b)
{
    new((void*)hndl) QJSValue((bool)b);
}

extern "C" int hsqml_is_jval_bool(HsQMLJValHandle* hndl)
{
    QJSValue* value = reinterpret_cast<QJSValue*>(hndl);
    return value->isBool();
}

extern "C" int hsqml_get_jval_bool(HsQMLJValHandle* hndl)
{
    QJSValue* value = reinterpret_cast<QJSValue*>(hndl);
    return value->toBool();
}

extern "C" void hsqml_init_jval_int(HsQMLJValHandle* hndl, int i)
{
    new((void*)hndl) QJSValue((int)i);
}

extern "C" void hsqml_init_jval_double(HsQMLJValHandle* hndl, double f)
{
    new((void*)hndl) QJSValue((double)f);
}

extern "C" int hsqml_is_jval_number(HsQMLJValHandle* hndl)
{
    QJSValue* value = reinterpret_cast<QJSValue*>(hndl);
    return value->isNumber();
}

extern "C" int hsqml_get_jval_int(HsQMLJValHandle* hndl)
{
    QJSValue* value = reinterpret_cast<QJSValue*>(hndl);
    return value->toInt();
}

extern "C" double hsqml_get_jval_double(HsQMLJValHandle* hndl)
{
    QJSValue* value = reinterpret_cast<QJSValue*>(hndl);
    return value->toNumber();
}

extern "C" void hsqml_init_jval_string(
    HsQMLJValHandle* hndl, HsQMLStringHandle* strh)
{
    QString* string = reinterpret_cast<QString*>(strh);
    new((void*)hndl) QJSValue(*string);
}
extern "C" int hsqml_is_jval_string(HsQMLJValHandle* hndl)
{
    QJSValue* value = reinterpret_cast<QJSValue*>(hndl);
    return value->isString();
}

extern "C" void hsqml_get_jval_string(
    HsQMLJValHandle* hndl, HsQMLStringHandle* strh)
{
    QJSValue* value = reinterpret_cast<QJSValue*>(hndl);
    QString* string = reinterpret_cast<QString*>(strh);
    *string = value->toString();
}

/* Array */
extern "C" void hsqml_init_jval_array(HsQMLJValHandle* hndl, unsigned int len)
{
    HsQMLEngine* engine = gManager->activeEngine();
    Q_ASSERT(engine);
    new((void*)hndl) QJSValue(engine->declEngine()->newArray(len));
}

extern "C" int hsqml_is_jval_array(HsQMLJValHandle* hndl)
{
    QJSValue* value = reinterpret_cast<QJSValue*>(hndl);
    return value->isArray();
}

extern "C" unsigned int hsqml_get_jval_array_length(HsQMLJValHandle* hndl)
{
    QJSValue* value = reinterpret_cast<QJSValue*>(hndl);
    return value->property("length").toUInt();
}

extern "C" void hsqml_jval_array_get(
    HsQMLJValHandle* ahndl, unsigned int i, HsQMLJValHandle* hndl)
{
    QJSValue* array = reinterpret_cast<QJSValue*>(ahndl);
    QJSValue* value = reinterpret_cast<QJSValue*>(hndl);
    *value = array->property(i);
}

extern "C" void hsqml_jval_array_set(
    HsQMLJValHandle* ahndl, unsigned int i, HsQMLJValHandle* hndl)
{
    QJSValue* array = reinterpret_cast<QJSValue*>(ahndl);
    QJSValue* value = reinterpret_cast<QJSValue*>(hndl);
    array->setProperty(i, *value);
}
