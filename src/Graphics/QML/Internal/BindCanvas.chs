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

type SyncCb = HsQMLJValHandle -> IO ()
type PaintCb = CDouble -> CDouble -> IO ()
type InitCb = Ptr (FunPtr SyncCb) -> Ptr (FunPtr PaintCb) -> IO ()
type CanvasFactory = IO (SyncCb, PaintCb)

foreign import ccall "wrapper"  
  marshalSyncCb :: SyncCb -> IO (FunPtr SyncCb)

foreign import ccall "wrapper"  
  marshalPaintCb :: PaintCb -> IO (FunPtr PaintCb)

foreign import ccall "wrapper"
  marshalInitCb :: InitCb -> IO (FunPtr InitCb)

withCanvasFactory :: CanvasFactory -> (FunPtr InitCb -> IO a) -> IO a
withCanvasFactory factory with = do
    let initFn syncPtrFPtr paintPtrFPtr = do
            (syncFn, paintFn) <- factory
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
   withCanvasFactory* `CanvasFactory'} ->
  `()' #}

{#fun unsafe hsqml_gldelegate_to_jval as ^
  {withHsQMLGLDelegateHandle* `HsQMLGLDelegateHandle',
   id `HsQMLJValHandle'} ->
  `()' #}

{#fun unsafe hsqml_gldelegate_from_jval as ^
  {withHsQMLGLDelegateHandle* `HsQMLGLDelegateHandle',
   id `HsQMLJValHandle'} ->
  `()' #}
