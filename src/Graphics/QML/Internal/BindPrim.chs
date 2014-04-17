{-# LANGUAGE
    ForeignFunctionInterface
  #-}

module Graphics.QML.Internal.BindPrim where

import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
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

withStrHndl :: (HsQMLStringHandle -> IO b) -> IO b
withStrHndl contFn =
    allocaBytes hsqmlStringSize $ \ptr -> do
        let str = HsQMLStringHandle ptr
        hsqmlInitString str
        ret <- contFn str
        hsqmlDeinitString str
        return ret
