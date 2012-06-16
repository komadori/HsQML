{-# LANGUAGE
    ForeignFunctionInterface
  #-}

module Graphics.QML.Internal.PrimValues where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import System.IO.Unsafe

#include "hsqml.h"

cIntConv :: (Integral a, Integral b) => a -> b
cIntConv = fromIntegral

--
-- String
--

{#pointer *HsQMLStringHandle as ^ newtype #}

{#fun unsafe hsqml_get_string_size as ^
  {} ->
  `Int' fromIntegral #}

hsqmlStringSize :: Int
hsqmlStringSize = unsafePerformIO $ hsqmlGetStringSize

{#fun unsafe hsqml_init_string as ^
  {id `HsQMLStringHandle'} ->
  `()' #}

{#fun unsafe hsqml_deinit_string as ^
  {id `HsQMLStringHandle'} ->
  `()' #}

{#fun unsafe hsqml_marshal_string as ^
  {`Int',
   id `HsQMLStringHandle'} ->
  `Ptr CUShort' id #}

{#fun unsafe hsqml_unmarshal_string as ^
  {id `HsQMLStringHandle',
   id `Ptr (Ptr CUShort)'} ->
  `Int' #}

--
-- URL
--

{#pointer *HsQMLUrlHandle as ^ newtype #}

{#fun unsafe hsqml_get_url_size as ^
  {} ->
  `Int' fromIntegral #}

hsqmlUrlSize :: Int
hsqmlUrlSize = unsafePerformIO $ hsqmlGetUrlSize

{#fun unsafe hsqml_init_url as ^
  {id `HsQMLUrlHandle'} ->
  `()' #}

{#fun unsafe hsqml_deinit_url as ^
  {id `HsQMLUrlHandle'} ->
  `()' #}

{#fun unsafe hsqml_marshal_url as ^
  {id `Ptr CChar',
   `Int',
   id `HsQMLUrlHandle'} ->
  `()' #}

{#fun unsafe hsqml_unmarshal_url as ^
  {id `HsQMLUrlHandle',
   id `Ptr (Ptr CChar)'} ->
  `Int' #}
