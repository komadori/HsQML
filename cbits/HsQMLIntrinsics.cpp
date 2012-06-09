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

extern "C" void hsqml_marshal_url(
  char* buf, int bufLen, HsQMLUrlHandle* hndl)
{
  QUrl* url = (QUrl*)hndl;
  QByteArray cstr;
  cstr.setRawData(buf, bufLen);
  *url = QUrl::fromEncoded(cstr);
}

extern "C" int hsqml_unmarshal_url(
  HsQMLUrlHandle* hndl, char** bufPtr)
{
  QUrl* url = (QUrl*)hndl;
  QByteArray cstr = url->toEncoded();
  int bufLen = cstr.length();
  char* buf = reinterpret_cast<char*>(malloc(bufLen));
  memcpy(buf, cstr.data(), bufLen);
  *bufPtr = buf;
  return bufLen;
}
