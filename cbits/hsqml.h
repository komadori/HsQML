#ifndef HSQML_H
#define HSQML_H

#ifdef __cplusplus
extern "C" {
#endif

#include <wchar.h>
#include <HsFFI.h>

/* Manager */
extern void hsqml_init(
  void (*)(HsFunPtr),
  void (*)(HsStablePtr));

extern void hsqml_run();

/* String */
typedef char HsQMLStringHandle;

extern const int hsqml_string_size;

extern void hsqml_init_string(
  HsQMLStringHandle*);

extern void hsqml_deinit_string(
  HsQMLStringHandle*);

extern void hsqml_marshal_string(
  const wchar_t*, int, HsQMLStringHandle*);

extern int hsqml_unmarshal_string_maxlen(
  HsQMLStringHandle*);

extern int hsqml_unmarshal_string(
  HsQMLStringHandle*, wchar_t*);

/* URL */
typedef char HsQMLUrlHandle;

extern const int hsqml_url_size;

extern void hsqml_init_url(
  HsQMLUrlHandle*);

extern void hsqml_deinit_url(
  HsQMLUrlHandle*);

extern void hsqml_string_to_url(
  HsQMLStringHandle*, HsQMLUrlHandle*);

extern void hsqml_url_to_string(
  HsQMLUrlHandle*, HsQMLStringHandle*);

/* Class */
typedef char HsQMLClassHandle;

typedef void (*HsQMLUniformFunc)(void*, void**);

extern HsQMLClassHandle* hsqml_create_class(
  unsigned int*, char*, HsQMLUniformFunc*, HsQMLUniformFunc*);

extern void hsqml_finalise_class_handle(
  HsQMLClassHandle* hndl);

/* Object */
typedef char HsQMLObjectHandle;

extern HsQMLObjectHandle* hsqml_create_object(
  HsStablePtr, HsQMLClassHandle*);

extern HsStablePtr hsqml_object_get_haskell(
  HsQMLObjectHandle*);

extern void* hsqml_object_get_pointer(
  HsQMLObjectHandle*);

extern HsQMLObjectHandle* hsqml_get_object_handle(
  void*);

extern void hsqml_finalise_object_handle(
  HsQMLObjectHandle*);

/* Engine */
extern void hsqml_create_engine(
  HsQMLObjectHandle*,
  const char*);

#ifdef __cplusplus
}
#endif

#endif /*HSQML_H*/
