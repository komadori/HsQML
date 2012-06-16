#ifndef HSQML_H
#define HSQML_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <HsFFI.h>

/* Manager */
extern void hsqml_init(
    void (*)(HsFunPtr),
    void (*)(HsStablePtr));

extern void hsqml_run();

/* String */
typedef char HsQMLStringHandle;

typedef unsigned short UTF16;

extern size_t hsqml_get_string_size();

extern void hsqml_init_string(
    HsQMLStringHandle*);

extern void hsqml_deinit_string(
    HsQMLStringHandle*);

extern UTF16* hsqml_marshal_string(
    int, HsQMLStringHandle*);

extern int hsqml_unmarshal_string(
    HsQMLStringHandle*, UTF16**);

/* URL */
typedef char HsQMLUrlHandle;

extern size_t hsqml_get_url_size();

extern void hsqml_init_url(
    HsQMLUrlHandle*);

extern void hsqml_deinit_url(
    HsQMLUrlHandle*);

extern void hsqml_marshal_url(
    char*, int, HsQMLUrlHandle*);

extern int hsqml_unmarshal_url(
    HsQMLUrlHandle*, char**);

/* Class */
typedef char HsQMLClassHandle;

typedef void (*HsQMLUniformFunc)(void*, void**);

extern int hsqml_get_next_class_id();

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
    void*, HsQMLClassHandle*);

extern void hsqml_finalise_object_handle(
    HsQMLObjectHandle*);

/* Engine */
extern void hsqml_create_engine(
    HsQMLObjectHandle*,
    HsQMLUrlHandle*,
    int,
    int,
    HsQMLStringHandle*);

#ifdef __cplusplus
}
#endif

#endif /*HSQML_H*/
