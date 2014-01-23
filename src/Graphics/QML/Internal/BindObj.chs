{-# LANGUAGE
    ForeignFunctionInterface
  #-}

module Graphics.QML.Internal.BindObj where

import Control.Exception (bracket_)
import Data.Typeable
import Foreign.C.Types
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

type UniformFunc = Ptr () -> Ptr (Ptr ()) -> IO ()

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
   marshalStable* `TypeRep',
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
 `()' #}

withActiveObject :: HsQMLObjectHandle -> IO () -> IO ()
withActiveObject hndl action =
    bracket_
        (hsqmlObjectSetActive $ Just hndl)
        (hsqmlObjectSetActive Nothing)
        action

{#fun unsafe hsqml_object_get_haskell as ^
  {withHsQMLObjectHandle* `HsQMLObjectHandle'} ->
  `a' fromStable* #}

{#fun unsafe hsqml_object_get_hs_typerep as ^
  {withHsQMLObjectHandle* `HsQMLObjectHandle'} ->
  `TypeRep' fromStable* #}

{#fun unsafe hsqml_object_get_pointer as ^
  {withHsQMLObjectHandle* `HsQMLObjectHandle'} ->
  `Ptr ()' id #}

{#fun unsafe hsqml_get_object_handle as ^
  {id `Ptr ()'} ->
  `HsQMLObjectHandle' newObjectHandle* #}

{#fun hsqml_fire_signal as ^
  {withHsQMLObjectHandle* `HsQMLObjectHandle',
   `Int',
   id `Ptr (Ptr ())'} ->
  `()' #}
