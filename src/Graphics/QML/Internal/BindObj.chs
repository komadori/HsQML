{-# LANGUAGE
    ForeignFunctionInterface
  #-}

module Graphics.QML.Internal.BindObj where

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

{#fun unsafe hsqml_fire_signal as ^
  {withHsQMLObjectHandle* `HsQMLObjectHandle',
   `Int',
   id `Ptr (Ptr ())'} ->
  `()' #}

ofDynamicMetaObject :: CUInt
ofDynamicMetaObject = 0x01

mfAccessPrivate, mfAccessProtected, mfAccessPublic, mfAccessMask,
  mfMethodMethod, mfMethodSignal, mfMethodSlot, mfMethodConstructor,
  mfMethodTypeMask, mfMethodCompatibility, mfMethodCloned, mfMethodScriptable
  :: CUInt
mfAccessPrivate   = 0x00
mfAccessProtected = 0x01
mfAccessPublic    = 0x02
mfAccessMask      = 0x03
mfMethodMethod      = 0x00
mfMethodSignal      = 0x04
mfMethodSlot        = 0x08
mfMethodConstructor = 0x0c
mfMethodTypeMask    = 0x0c
mfMethodCompatibility = 0x10
mfMethodCloned        = 0x20
mfMethodScriptable    = 0x40

pfInvalid, pfReadable, pfWritable, pfResettable, pfEnumOrFlag, pfStdCppSet,
  pfConstant, pfFinal, pfDesignable, pfResolveDesignable, pfScriptable,
  pfResolveScriptable, pfStored, pfResolveStored, pfEditable,
  pfResolveEditable, pfUser, pfResolveUser, pfNotify :: CUInt
pfInvalid           = 0x00000000
pfReadable          = 0x00000001
pfWritable          = 0x00000002
pfResettable        = 0x00000004
pfEnumOrFlag        = 0x00000008
pfStdCppSet         = 0x00000100
pfConstant          = 0x00000400
pfFinal             = 0x00000800
pfDesignable        = 0x00001000
pfResolveDesignable = 0x00002000
pfScriptable        = 0x00004000
pfResolveScriptable = 0x00008000
pfStored            = 0x00010000
pfResolveStored     = 0x00020000
pfEditable          = 0x00040000
pfResolveEditable   = 0x00080000
pfUser              = 0x00100000
pfResolveUser       = 0x00200000
pfNotify            = 0x00400000
