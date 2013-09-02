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

extern void hsqml_set_debug_loglevel(int);

/* Event Loop */
typedef void (*HsQMLTrivialCb)();

typedef enum {
    HSQML_EVLOOP_OK = 0,
    HSQML_EVLOOP_ALREADY_RUNNING,
    HSQML_EVLOOP_WRONG_THREAD,
    HSQML_EVLOOP_NOT_RUNNING,
    HSQML_EVLOOP_OTHER_ERROR
} HsQMLEventLoopStatus;

extern HsQMLEventLoopStatus hsqml_evloop_run(
    HsQMLTrivialCb startCb,
    HsQMLTrivialCb jobsCb,
    HsQMLTrivialCb yieldCb);

extern HsQMLEventLoopStatus hsqml_evloop_require();

extern void hsqml_evloop_release();

extern void hsqml_evloop_notify_jobs();

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
    unsigned int*, char*, HsStablePtr, HsQMLUniformFunc*, HsQMLUniformFunc*);

extern void hsqml_finalise_class_handle(
    HsQMLClassHandle* hndl);

/* Object */
typedef char HsQMLObjectHandle;

extern HsQMLObjectHandle* hsqml_create_object(
    HsStablePtr, HsQMLClassHandle*);

extern void hsqml_object_set_active(
    HsQMLObjectHandle*);

extern HsStablePtr hsqml_object_get_hs_typerep(
    HsQMLObjectHandle*);

extern HsStablePtr hsqml_object_get_haskell(
    HsQMLObjectHandle*);

extern void* hsqml_object_get_pointer(
    HsQMLObjectHandle*);

extern HsQMLObjectHandle* hsqml_get_object_handle(
    void*);

extern void hsqml_finalise_object_handle(
    HsQMLObjectHandle*);

extern void hsqml_fire_signal(
    HsQMLObjectHandle*, int, void**);

/* Engine */
extern void hsqml_create_engine(
    HsQMLObjectHandle*,
    HsQMLUrlHandle*,
    int,
    int,
    HsQMLStringHandle*,
    HsQMLTrivialCb stopCb);

#ifdef __cplusplus
}
#endif

#endif /*HSQML_H*/
