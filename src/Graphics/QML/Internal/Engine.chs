{-# LANGUAGE
    ForeignFunctionInterface
  #-}
{-# OPTIONS_HADDOCK hide #-}

module Graphics.QML.Internal.Engine where

{#import Graphics.QML.Internal.Objects #}

import Foreign.C.Types
import Foreign.C.String
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

{#fun unsafe hsqml_create_engine as ^
  {withHsQMLObjectHandle* `HsQMLObjectHandle',
   `String'} ->
  `()' #}

{#fun hsqml_run as ^ {} -> `()' #}
