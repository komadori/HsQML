{-# LANGUAGE
    ForeignFunctionInterface
  #-}

module Graphics.QML.Internal.BindCanvas where

{#import Graphics.QML.Internal.BindPrim #}

import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.ForeignPtr.Safe
import Foreign.Storable
import System.IO.Unsafe

#include "hsqml.h"

--
-- GL Delegate
--

{#pointer *HsQMLGLDelegateHandle as ^ foreign newtype #}

type DeInitCb = IO ()
type SyncCb = HsQMLJValHandle -> IO ()
type PaintCb = CDouble -> CDouble -> IO ()
type InitCb = Ptr (FunPtr DeInitCb) -> Ptr (FunPtr DeInitCb) ->
    Ptr (FunPtr SyncCb) -> Ptr (FunPtr PaintCb) -> IO ()
type CallbacksFactory = IO (DeInitCb, DeInitCb, SyncCb, PaintCb)

foreign import ccall "wrapper"
  marshalDeInitCb :: DeInitCb -> IO (FunPtr DeInitCb)

foreign import ccall "wrapper"  
  marshalSyncCb :: SyncCb -> IO (FunPtr SyncCb)

foreign import ccall "wrapper"  
  marshalPaintCb :: PaintCb -> IO (FunPtr PaintCb)

foreign import ccall "wrapper"
  marshalInitCb :: InitCb -> IO (FunPtr InitCb)

withCallbacksFactory :: CallbacksFactory -> (FunPtr InitCb -> IO a) -> IO a
withCallbacksFactory factory with = do
    let initFn initPtrFPtr deinitPtrFPtr syncPtrFPtr paintPtrFPtr = do
            (initFn, deinitFn, syncFn, paintFn) <- factory
            initFPtr <- marshalDeInitCb initFn
            poke initPtrFPtr initFPtr
            deinitFPtr <- marshalDeInitCb deinitFn
            poke deinitPtrFPtr deinitFPtr
            syncFPtr <- marshalSyncCb syncFn
            poke syncPtrFPtr syncFPtr
            paintFPtr <- marshalPaintCb paintFn
            poke paintPtrFPtr paintFPtr
    initFPtr <- marshalInitCb initFn
    with initFPtr

foreign import ccall "hsqml.h &hsqml_finalise_gldelegate_handle"
    hsqmlFinaliseGldelegateHandlePtr ::
        FunPtr (Ptr HsQMLGLDelegateHandle -> IO ())

newGLDelegateHandle :: Ptr HsQMLGLDelegateHandle -> IO HsQMLGLDelegateHandle
newGLDelegateHandle p = do
    fp <- newForeignPtr hsqmlFinaliseGldelegateHandlePtr p
    return $ HsQMLGLDelegateHandle fp

{#fun unsafe hsqml_create_gldelegate as ^
  {} ->
  `HsQMLGLDelegateHandle' newGLDelegateHandle* #}

{#fun unsafe hsqml_gldelegate_setup as ^
  {withHsQMLGLDelegateHandle* `HsQMLGLDelegateHandle',
   withCallbacksFactory* `CallbacksFactory'} ->
  `()' #}

{#fun unsafe hsqml_gldelegate_to_jval as ^
  {withHsQMLGLDelegateHandle* `HsQMLGLDelegateHandle',
   id `HsQMLJValHandle'} ->
  `()' #}

{#fun unsafe hsqml_gldelegate_from_jval as ^
  {withHsQMLGLDelegateHandle* `HsQMLGLDelegateHandle',
   id `HsQMLJValHandle'} ->
  `()' #}
