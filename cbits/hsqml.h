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

extern UTF16* hsqml_write_string(
    int, HsQMLStringHandle*);

extern int hsqml_read_string(
    HsQMLStringHandle*, UTF16**);

/* JSValue */
typedef char HsQMLJValHandle;

extern size_t hsqml_get_jval_size();

extern int hsqml_get_jval_typeid();

extern void hsqml_init_jval_null(
    HsQMLJValHandle* hndl, int);

extern void hsqml_set_jval(
    HsQMLJValHandle*, HsQMLJValHandle*);

extern void hsqml_deinit_jval(
    HsQMLJValHandle*);

extern void hsqml_init_jval_bool(
    HsQMLJValHandle*, int);

extern int hsqml_is_jval_bool(
    HsQMLJValHandle* hndl);

extern int hsqml_get_jval_bool(
    HsQMLJValHandle* hndl);

extern void hsqml_init_jval_int(
    HsQMLJValHandle*, int);

extern void hsqml_init_jval_double(
    HsQMLJValHandle*, double);

extern int hsqml_is_jval_number(
    HsQMLJValHandle*);

extern int hsqml_get_jval_int(
    HsQMLJValHandle*);

extern double hsqml_get_jval_double(
    HsQMLJValHandle*);

extern void hsqml_init_jval_string(
    HsQMLJValHandle*, HsQMLStringHandle*);

extern int hsqml_is_jval_string(
    HsQMLJValHandle*);

extern void hsqml_get_jval_string(
    HsQMLJValHandle*, HsQMLStringHandle*);

/* Array */
extern void hsqml_init_jval_array(
    HsQMLJValHandle*, unsigned int);

extern int hsqml_is_jval_array(
    HsQMLJValHandle*);

extern unsigned int hsqml_get_jval_array_length(
    HsQMLJValHandle*);

extern void hsqml_jval_array_get(
    HsQMLJValHandle*, unsigned int, HsQMLJValHandle*);

extern void hsqml_jval_array_set(
    HsQMLJValHandle*, unsigned int, HsQMLJValHandle*);

/* Class */
typedef char HsQMLClassHandle;

typedef void (*HsQMLUniformFunc)(void*, void**);

extern int hsqml_get_next_class_id();

extern HsQMLClassHandle* hsqml_create_class(
    unsigned int*, unsigned int*, char*,
    HsStablePtr, HsQMLUniformFunc*, HsQMLUniformFunc*);

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

extern HsStablePtr hsqml_object_get_hs_value(
    HsQMLObjectHandle*);

extern void* hsqml_object_get_pointer(
    HsQMLObjectHandle*);

extern HsQMLJValHandle* hsqml_object_get_jval(
    HsQMLObjectHandle*);

extern HsQMLObjectHandle* hsqml_get_object_from_pointer(
    void*);

extern HsQMLObjectHandle* hsqml_get_object_from_jval(
    HsQMLJValHandle*);

extern void hsqml_finalise_object_handle(
    HsQMLObjectHandle*);

extern void hsqml_fire_signal(
    HsQMLObjectHandle*, int, void**);

/* Engine */
extern void hsqml_create_engine(
    HsQMLObjectHandle*,
    HsQMLStringHandle*,
    HsQMLTrivialCb stopCb);

#ifdef __cplusplus
}
#endif

#endif /*HSQML_H*/
