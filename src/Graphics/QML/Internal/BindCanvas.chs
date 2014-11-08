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

type SetupCb = CInt -> IO ()
type CleanupCb = IO ()
type SyncCb = HsQMLJValHandle -> IO CInt
type PaintCb = Ptr CFloat -> IO ()
type MakeCb = Ptr (FunPtr SetupCb) -> Ptr (FunPtr CleanupCb) ->
    Ptr (FunPtr SyncCb) -> Ptr (FunPtr PaintCb) -> IO ()
type CallbacksFactory = IO (SetupCb, CleanupCb, SyncCb, PaintCb)

{#enum HsQMLGLCanvasType as ^ {underscoreToCase} #}

foreign import ccall "wrapper"
  marshalSetupCb :: SetupCb -> IO (FunPtr SetupCb)

foreign import ccall "wrapper"
  marshalCleanupCb :: CleanupCb -> IO (FunPtr CleanupCb)

foreign import ccall "wrapper"  
  marshalSyncCb :: SyncCb -> IO (FunPtr SyncCb)

foreign import ccall "wrapper"  
  marshalPaintCb :: PaintCb -> IO (FunPtr PaintCb)

foreign import ccall "wrapper"
  marshalMakeCb :: MakeCb -> IO (FunPtr MakeCb)

withCallbacksFactory :: CallbacksFactory -> (FunPtr MakeCb -> IO a) -> IO a
withCallbacksFactory factory with = do
    let makeFn setupPtrFPtr cleanupPtrFPtr syncPtrFPtr paintPtrFPtr = do
            (setupFn, cleanupFn, syncFn, paintFn) <- factory
            setupFPtr <- marshalSetupCb setupFn
            poke setupPtrFPtr setupFPtr
            cleanupFPtr <- marshalCleanupCb cleanupFn
            poke cleanupPtrFPtr cleanupFPtr
            syncFPtr <- marshalSyncCb syncFn
            poke syncPtrFPtr syncFPtr
            paintFPtr <- marshalPaintCb paintFn
            poke paintPtrFPtr paintFPtr
    makeFPtr <- marshalMakeCb makeFn
    with makeFPtr

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
