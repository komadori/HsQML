{-# LANGUAGE
    ForeignFunctionInterface
  #-}

module Graphics.QML.Internal.BindObj where

import Graphics.QML.Internal.Types
{#import Graphics.QML.Internal.BindPrim #}

import Control.Exception (bracket)
import Control.Monad (void)
import Foreign.C.Types
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr
import Foreign.ForeignPtr.Safe
import Foreign.ForeignPtr.Unsafe
import Foreign.StablePtr

#include "hsqml.h"

marshalStable :: a -> (Ptr () -> IO b) -> IO b
marshalStable obj f = do
  sPtr <- newStablePtr obj
  res <- f $ castStablePtrToPtr sPtr
  return res

fromStable :: Ptr () -> IO a
fromStable =
  deRefStablePtr . castPtrToStablePtr

{#fun unsafe hsqml_get_next_class_id as ^
  {} ->
  `CInt' id #}

foreign import ccall "wrapper"  
  marshalFunc :: UniformFunc -> IO (FunPtr UniformFunc)

{#pointer *HsQMLClassHandle as ^ foreign newtype #}

foreign import ccall "hsqml.h &hsqml_finalise_class_handle"
  hsqmlFinaliseClassHandlePtr :: FunPtr (Ptr (HsQMLClassHandle) -> IO ())

newClassHandle :: Ptr HsQMLClassHandle -> IO (Maybe HsQMLClassHandle)
newClassHandle p =
  if nullPtr == p
    then return Nothing
    else do
      fp <- newForeignPtr hsqmlFinaliseClassHandlePtr p
      return $ Just $ HsQMLClassHandle fp

{#fun unsafe hsqml_create_class as ^
  {id `Ptr CUInt',
   id `Ptr CUInt',
   id `Ptr CChar',
   marshalStable* `ClassInfo',
   id `Ptr (FunPtr UniformFunc)',
   id `Ptr (FunPtr UniformFunc)'} ->
  `Maybe HsQMLClassHandle' newClassHandle* #}

withMaybeHsQMLObjectHandle ::
    Maybe HsQMLObjectHandle -> (Ptr HsQMLObjectHandle -> IO b) -> IO b
withMaybeHsQMLObjectHandle (Just (HsQMLObjectHandle fp)) = withForeignPtr fp
withMaybeHsQMLObjectHandle Nothing = \f -> f nullPtr

{#pointer *HsQMLObjectHandle as ^ foreign newtype #}

foreign import ccall "hsqml.h &hsqml_finalise_object_handle"
  hsqmlFinaliseObjectHandlePtr :: FunPtr (Ptr (HsQMLObjectHandle) -> IO ())

foreign import ccall "hsqml.h &hsqml_finalise_object_weak_handle"
  hsqmlFinaliseObjectWeakHandlePtr :: FunPtr (Ptr (HsQMLObjectHandle) -> IO ())

newObjectHandle :: Ptr HsQMLObjectHandle -> IO HsQMLObjectHandle
newObjectHandle p = do
  fp <- newForeignPtr hsqmlFinaliseObjectHandlePtr p
  return $ HsQMLObjectHandle fp

isNullObjectHandle :: HsQMLObjectHandle -> Bool
isNullObjectHandle (HsQMLObjectHandle fp) =
  nullPtr == unsafeForeignPtrToPtr fp

{#fun unsafe hsqml_create_object as ^
  {marshalStable* `a',
   withHsQMLClassHandle* `HsQMLClassHandle'} ->
  `HsQMLObjectHandle' newObjectHandle* #}

{#fun unsafe hsqml_object_set_active as ^
  {withMaybeHsQMLObjectHandle* `Maybe HsQMLObjectHandle'} ->
  `Bool' toBool #}

withActiveObject :: HsQMLObjectHandle -> IO () -> IO ()
withActiveObject hndl action =
    bracket
        (hsqmlObjectSetActive $ Just hndl)
        (\ok -> if ok then void $ hsqmlObjectSetActive Nothing else return ())
        (\ok -> if ok then action else return ())

{#fun unsafe hsqml_object_get_hs_typerep as ^
  {withHsQMLObjectHandle* `HsQMLObjectHandle'} ->
  `ClassInfo' fromStable* #}

{#fun unsafe hsqml_object_get_hs_value as ^
  {withHsQMLObjectHandle* `HsQMLObjectHandle'} ->
  `a' fromStable* #}

{#fun unsafe hsqml_object_get_pointer as ^
  {withHsQMLObjectHandle* `HsQMLObjectHandle'} ->
  `Ptr ()' id #}

{#fun unsafe hsqml_object_get_jval as ^
  {withHsQMLObjectHandle* `HsQMLObjectHandle'} ->
  `HsQMLJValHandle' id #}

{#fun unsafe hsqml_get_object_from_pointer as ^
  {id `Ptr ()'} ->
  `HsQMLObjectHandle' newObjectHandle* #}

{#fun unsafe hsqml_get_object_from_jval as ^
  {id `HsQMLJValHandle'} ->
  `HsQMLObjectHandle' newObjectHandle* #}

{#fun unsafe hsqml_object_reference_handle as ^
  {id `Ptr HsQMLObjectHandle',
   fromBool `Bool'} ->
  `()' #}

copyObjectHandle :: HsQMLObjectHandle -> Bool -> IO HsQMLObjectHandle
copyObjectHandle (HsQMLObjectHandle fp) weak = do
    withForeignPtr fp $ \p -> do
        hsqmlObjectReferenceHandle p weak
        fp' <- newForeignPtr final p
        return $ HsQMLObjectHandle fp'
    where final = if weak
                  then hsqmlFinaliseObjectWeakHandlePtr
                  else hsqmlFinaliseObjectHandlePtr

{#fun hsqml_fire_signal as ^
  {withHsQMLObjectHandle* `HsQMLObjectHandle',
   `Int',
   id `Ptr (Ptr ())'} ->
  `()' #}

{#pointer *HsQMLObjFinaliserHandle as ^ foreign newtype #}

foreign import ccall "hsqml.h &hsqml_finalise_obj_finaliser"
  hsqmlFinaliseObjFinaliserPtr :: FunPtr (Ptr HsQMLObjFinaliserHandle -> IO ())

type ObjFinaliserFunc = Ptr HsQMLObjectHandle -> IO ()

foreign import ccall "wrapper"  
  marshalObjFinaliser :: ObjFinaliserFunc -> IO (FunPtr ObjFinaliserFunc)

newObjFinaliserHandle ::
    Ptr HsQMLObjFinaliserHandle -> IO HsQMLObjFinaliserHandle
newObjFinaliserHandle p = do
    fp <- newForeignPtr hsqmlFinaliseObjFinaliserPtr p
    return $ HsQMLObjFinaliserHandle fp

{#fun unsafe hsqml_create_obj_finaliser as ^
  {id `FunPtr ObjFinaliserFunc'} ->
  `HsQMLObjFinaliserHandle' newObjFinaliserHandle* #}

{#fun unsafe hsqml_object_add_finaliser as ^
  {withHsQMLObjectHandle* `HsQMLObjectHandle',
   withHsQMLObjFinaliserHandle* `HsQMLObjFinaliserHandle'} ->
  `()' #}

