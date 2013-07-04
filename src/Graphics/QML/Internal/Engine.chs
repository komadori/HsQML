{-# LANGUAGE
    ForeignFunctionInterface
  #-}
{-# OPTIONS_HADDOCK hide #-}

module Graphics.QML.Internal.Engine where

{#import Graphics.QML.Internal.BindObj #}

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.StablePtr

#include <HsFFI.h>

#include "hsqml.h"

type HsFreeFunPtr = FunPtr (FunPtr (IO ()) -> IO ())
foreign import ccall "HsFFI.h &hs_free_fun_ptr"
  hsFreeFunPtr :: HsFreeFunPtr

type HsFreeStablePtr = FunPtr (Ptr () -> IO ())
foreign import ccall "HsFFI.h &hs_free_stable_ptr"
  hsFreeStablePtr :: HsFreeStablePtr

{#fun unsafe hsqml_init as hsqmlInit_
  {id `HsFreeFunPtr',
   id `HsFreeStablePtr'} ->
  `()' #}

hsqmlInit :: IO ()
hsqmlInit = hsqmlInit_ hsFreeFunPtr hsFreeStablePtr

type EngineStopCb = IO ()

foreign import ccall "wrapper"  
  marshalEngStopCb :: EngineStopCb -> IO (FunPtr EngineStopCb)

withEngineStopCb :: EngineStopCb -> (FunPtr EngineStopCb -> IO a) -> IO a
withEngineStopCb f with = marshalEngStopCb f >>= with

{#fun hsqml_run_engine as ^
  {withMaybeHsQMLObjectHandle* `Maybe HsQMLObjectHandle',
   castPtr `Ptr ()',
   fromBool `Bool',
   fromBool `Bool',
   castPtr `Ptr ()',
   withEngineStopCb* `EngineStopCb'} ->
  `Int' fromIntegral #}

{#fun unsafe hsqml_set_debug_loglevel as ^
  {fromIntegral `Int'} -> `()'
  #}
