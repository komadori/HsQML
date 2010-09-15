{-# LANGUAGE
    ForeignFunctionInterface
  #-}
{-# OPTIONS_HADDOCK hide #-}

module Graphics.QML.Internal.Engine where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import System.IO.Unsafe

#include "hsqml.h"

{#fun unsafe hsqml_create_engine as ^
  {id `Ptr ()',
   `String'} ->
  `()' #}

{#fun hsqml_run as ^ {} -> `()' #}
