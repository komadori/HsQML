#include <cstdlib>
#include <cstring>

#include <QString>
#include <QUrl>

#include "hsqml.h"

/* String */
extern "C" const int hsqml_string_size = sizeof(QString);

extern "C" void hsqml_init_string(HsQMLStringHandle* hndl)
{
  new((void*)hndl) QString();
}

extern "C" void hsqml_deinit_string(HsQMLStringHandle* hndl)
{
  QString* string = (QString*)hndl;
  string->~QString();
}

extern "C" void hsqml_marshal_string(
  const wchar_t* buf, int bufLen, HsQMLStringHandle* hndl)
{
  QString* string = (QString*)hndl;
  *string = QString::fromWCharArray(buf, bufLen);
}

extern "C" int hsqml_unmarshal_string_maxlen(
  HsQMLStringHandle* hndl)
{
  QString* string = (QString*)hndl;
  return string->length();
}

extern "C" int hsqml_unmarshal_string(
  HsQMLStringHandle* hndl, wchar_t* buf)
{
  QString* string = (QString*)hndl;
  return string->toWCharArray(buf);
}

/* URL */
extern "C" const int hsqml_url_size = sizeof(QUrl);

extern "C" void hsqml_init_url(HsQMLUrlHandle* hndl)
{
  new((void*)hndl) QUrl();
}

extern "C" void hsqml_deinit_url(HsQMLUrlHandle* hndl)
{
  QUrl* url = (QUrl*)hndl;
  url->~QUrl();
}

extern "C" void hsqml_string_to_url(
  HsQMLStringHandle* shndl, HsQMLUrlHandle* uhndl)
{
  QString* string = (QString*)shndl;
  QUrl* url = (QUrl*)uhndl;
  *url = QUrl(*string);
}

extern "C" void hsqml_url_to_string(
  HsQMLUrlHandle* uhndl, HsQMLStringHandle* shndl)
{
  QUrl* url = (QUrl*)uhndl;
  QString* string = (QString*)shndl;
  *string = url->toString();
}
