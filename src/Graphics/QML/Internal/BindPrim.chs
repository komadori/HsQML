{-# LANGUAGE
    ForeignFunctionInterface
  #-}

module Graphics.QML.Internal.BindPrim where

import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
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

{#fun unsafe hsqml_write_string as ^
  {`Int',
   id `HsQMLStringHandle'} ->
  `Ptr CUShort' id #}

{#fun unsafe hsqml_read_string as ^
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

--
-- JSValue
--

{#pointer *HsQMLJValHandle as ^ newtype #}

{#fun unsafe hsqml_get_jval_size as ^
  {} ->
  `Int' fromIntegral #}

hsqmlJValSize :: Int
hsqmlJValSize = unsafePerformIO $ hsqmlGetJvalSize

{#fun unsafe hsqml_get_jval_typeid as ^
  {} ->
  `Int' fromIntegral #}

hsqmlJValTypeId :: Int
hsqmlJValTypeId = unsafePerformIO $ hsqmlGetJvalTypeid

{#fun unsafe hsqml_init_jval_null as ^
  {id `HsQMLJValHandle',
   fromBool `Bool'} ->
  `()' #}

{#fun unsafe hsqml_deinit_jval as ^
  {id `HsQMLJValHandle'} ->
  `()' #}

{#fun unsafe hsqml_set_jval as ^
  {id `HsQMLJValHandle',
   id `HsQMLJValHandle'} ->
  `()' #}

{#fun unsafe hsqml_init_jval_bool as ^
  {id `HsQMLJValHandle',
   fromBool `Bool'} ->
  `()' #}

{#fun unsafe hsqml_is_jval_bool as ^
  {id `HsQMLJValHandle'} ->
  `Bool' toBool #}

{#fun unsafe hsqml_get_jval_bool as ^
  {id `HsQMLJValHandle'} ->
  `Bool' toBool #}

{#fun unsafe hsqml_init_jval_int as ^
  {id `HsQMLJValHandle',
   id `CInt'} ->
  `()' #}

{#fun unsafe hsqml_init_jval_double as ^
  {id `HsQMLJValHandle',
   id `CDouble'} ->
  `()' #}

{#fun unsafe hsqml_is_jval_number as ^
  {id `HsQMLJValHandle'} ->
  `Bool' toBool #}

{#fun unsafe hsqml_get_jval_int as ^
  {id `HsQMLJValHandle'} ->
  `CInt' id #}

{#fun unsafe hsqml_get_jval_double as ^
  {id `HsQMLJValHandle'} ->
  `CDouble' id #}

{#fun unsafe hsqml_init_jval_string as ^
  {id `HsQMLJValHandle',
   id `HsQMLStringHandle'} ->
  `()' #}

{#fun unsafe hsqml_is_jval_string as ^
  {id `HsQMLJValHandle'} ->
  `Bool' toBool #}

{#fun unsafe hsqml_get_jval_string as ^
  {id `HsQMLJValHandle',
   id `HsQMLStringHandle'} ->
  `()' #}

fromJVal ::
    (HsQMLJValHandle -> IO Bool) -> (HsQMLJValHandle -> IO a) ->
    HsQMLJValHandle -> IO (Maybe a)
fromJVal isFn getFn jval = do
    is <- isFn jval
    if is then fmap Just $ getFn jval else return Nothing

withJVal ::
    (HsQMLJValHandle -> a -> IO ()) -> a -> (HsQMLJValHandle -> IO b) -> IO b
withJVal initFn val contFn =
    allocaBytes hsqmlJValSize $ \ptr -> do
        let jval = HsQMLJValHandle ptr
        initFn jval val
        ret <- contFn jval
        hsqmlDeinitJval jval
        return ret
