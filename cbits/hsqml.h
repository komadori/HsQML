#ifndef HSQML_H
#define HSQML_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <HsFFI.h>

/* Init */
extern void hsqml_init(
    void (*)(HsFunPtr),
    void (*)(HsStablePtr));

/* Event Loop */
typedef void (*HsQMLTrivialCb)();

typedef enum {
    HSQML_EVLOOP_OK = 0,
    HSQML_EVLOOP_ALREADY_RUNNING,
    HSQML_EVLOOP_POST_SHUTDOWN,
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

extern HsQMLEventLoopStatus hsqml_evloop_shutdown();

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

extern int hsqml_object_set_active(
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

extern void hsqml_object_reference_handle(
    HsQMLObjectHandle*, int);

extern void hsqml_finalise_object_handle(
    HsQMLObjectHandle*);

extern void hsqml_finalise_object_weak_handle(
    HsQMLObjectHandle*);

extern void hsqml_fire_signal(
    HsQMLObjectHandle*, int, void**);

/* Object Finaliser */
typedef char HsQMLObjFinaliserHandle;

typedef void (*HsQMLObjFinaliserCb)(HsQMLObjectHandle*);

extern HsQMLObjFinaliserHandle* hsqml_create_obj_finaliser(
    HsQMLObjFinaliserCb);

extern void hsqml_finalise_obj_finaliser(
    HsQMLObjFinaliserHandle*);

extern void hsqml_object_add_finaliser(
    HsQMLObjectHandle*, HsQMLObjFinaliserHandle*);

/* Global */
extern int hsqml_set_args(HsQMLStringHandle**);

extern void hsqml_set_debug_loglevel(int);

/* Engine */
extern void hsqml_create_engine(
    HsQMLObjectHandle*,
    HsQMLStringHandle*,
    HsQMLStringHandle**,
    HsQMLStringHandle**,
    HsQMLTrivialCb stopCb);

/* Canvas */
typedef char HsQMLGLDelegateHandle;

typedef enum {
    HSQML_GL_DESKTOP,
    HSQML_GL_ES
} HsQMLGLCanvasType;

typedef void (*HsQMLGLSetupCb)(
    HsQMLGLCanvasType, int, int);

typedef void (*HsQMLGLCleanupCb)();

typedef int (*HsQMLGLSyncCb)(
    HsQMLJValHandle*);

typedef void (*HsQMLGLPaintCb)(
    float*, float, float);

typedef void (*HsQMLGLMakeCallbacksCb)(
    HsQMLGLSetupCb*,
    HsQMLGLCleanupCb*,
    HsQMLGLSyncCb*,
    HsQMLGLPaintCb*);

extern HsQMLGLDelegateHandle* hsqml_create_gldelegate();

extern void hsqml_finalise_gldelegate_handle(
    HsQMLGLDelegateHandle*);

extern void hsqml_gldelegate_setup(
    HsQMLGLDelegateHandle*,
    HsQMLGLMakeCallbacksCb);

extern void hsqml_gldelegate_to_jval(
    HsQMLGLDelegateHandle*,
    HsQMLJValHandle*);

extern int hsqml_gldelegate_from_jval(
    HsQMLGLDelegateHandle*,
    HsQMLJValHandle*);

#ifdef __cplusplus
}
#endif

#endif /*HSQML_H*/
