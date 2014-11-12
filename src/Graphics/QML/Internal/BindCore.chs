{-# LANGUAGE
    ForeignFunctionInterface
  #-}
{-# OPTIONS_HADDOCK hide #-}

module Graphics.QML.Internal.BindCore where

{#import Graphics.QML.Internal.BindPrim #}
{#import Graphics.QML.Internal.BindObj #}

import Foreign.C.Types
import Foreign.Ptr

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

type TrivialCb = IO ()

foreign import ccall "wrapper"  
  marshalTrivialCb :: TrivialCb -> IO (FunPtr TrivialCb)

withTrivialCb :: TrivialCb -> (FunPtr TrivialCb -> IO a) -> IO a
withTrivialCb f with = marshalTrivialCb f >>= with

withMaybeTrivialCb :: Maybe TrivialCb -> (FunPtr TrivialCb -> IO b) -> IO b
withMaybeTrivialCb (Just f) = withTrivialCb f
withMaybeTrivialCb Nothing = \cont -> cont nullFunPtr

{#enum HsQMLEventLoopStatus as ^ {underscoreToCase} #}

{#fun hsqml_evloop_run as ^
  {withTrivialCb* `TrivialCb',
   withTrivialCb* `TrivialCb',
   withMaybeTrivialCb* `Maybe TrivialCb'} ->
  `HsQMLEventLoopStatus' cIntToEnum #}

{#fun hsqml_evloop_require as ^
  {} ->
  `HsQMLEventLoopStatus' cIntToEnum #}

{#fun hsqml_evloop_release as ^
  {} ->
  `()' #}

{#fun unsafe hsqml_evloop_notify_jobs as ^
  {} ->
  `()' #}

{#fun hsqml_create_engine as ^
  {withMaybeHsQMLObjectHandle* `Maybe HsQMLObjectHandle',
   id `HsQMLStringHandle',
   withTrivialCb* `TrivialCb'} ->
  `()' #}

{#fun unsafe hsqml_set_debug_loglevel as ^
  {fromIntegral `Int'} -> `()'
  #}
