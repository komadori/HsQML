{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies
  #-}

module Graphics.QML.Internal.Objects where

import Graphics.QML.Internal.BindObj
import Graphics.QML.Internal.Marshal
import Graphics.QML.Internal.Types

import Control.Monad.Trans.Maybe
import Data.Tagged
import Data.Typeable
import Foreign.ForeignPtr

-- | Represents an instance of the QML class which wraps the type @tt@.
newtype ObjRef tt = ObjRef HsQMLObjectHandle

instance (Typeable tt) => Marshal (ObjRef tt) where
    type MarshalMode (ObjRef tt) c d = ModeObjBidi tt c
    marshaller = Marshaller {
        mTypeCVal_ = retag (mTypeCVal :: Tagged AnyObjRef TypeId),
        mFromCVal_ = \ptr -> do
            anyObj <- mFromCVal ptr
            MaybeT $ fromAnyObjRefIO anyObj,
        mToCVal_ = \(ObjRef hndl) ptr ->
            mToCVal (AnyObjRef hndl) ptr,
        mWithCVal_ = \(ObjRef hndl) f ->
            mWithCVal (AnyObjRef hndl) f,
        mFromJVal_ = \ptr -> do
            anyObj <- mFromJVal ptr
            MaybeT $ fromAnyObjRefIO anyObj,
        mWithJVal_ = \(ObjRef hndl) f ->
            mWithJVal (AnyObjRef hndl) f,
        mFromHndl_ =
            return . ObjRef,
        mToHndl_ = \(ObjRef hndl) ->
            return hndl}

fromObjRefIO :: ObjRef tt -> IO tt
fromObjRefIO (ObjRef hndl) = hsqmlObjectGetHsValue hndl

-- | Represents an instance of a QML class which wraps an arbitrary Haskell
-- type. Unlike 'ObjRef', an 'AnyObjRef' only carries the type of its Haskell
-- value dynamically and does not encode it into the static type.
newtype AnyObjRef = AnyObjRef HsQMLObjectHandle

instance Marshal AnyObjRef where
    type MarshalMode AnyObjRef c d = ModeObjBidi No c
    marshaller = Marshaller {
        mTypeCVal_ = Tagged tyJSValue,
        mFromCVal_ = jvalFromCVal,
        mToCVal_ = jvalToCVal,
        mWithCVal_ = jvalWithCVal,
        mFromJVal_ = \ptr -> MaybeT $ do
            hndl <- hsqmlGetObjectFromJval ptr
            return $ if isNullObjectHandle hndl
                then Nothing else Just $ AnyObjRef hndl,
        mWithJVal_ = \(AnyObjRef hndl@(HsQMLObjectHandle ptr)) f -> do
            jval <- hsqmlObjectGetJval hndl
            ret <- f jval
            touchForeignPtr ptr
            return ret,
        mFromHndl_ =
            return . AnyObjRef,
        mToHndl_ = \(AnyObjRef hndl) ->
            return hndl}

fromAnyObjRefIO :: forall tt. (Typeable tt) =>
    AnyObjRef -> IO (Maybe (ObjRef tt))
fromAnyObjRefIO (AnyObjRef hndl) = do
    info <- hsqmlObjectGetHsTyperep hndl
    let srcRep = typeOf (undefined :: tt)
        dstRep = cinfoObjType info
    return $ if srcRep == dstRep
        then Just $ ObjRef hndl
        else Nothing
