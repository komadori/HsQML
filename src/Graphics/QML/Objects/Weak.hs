-- | Facilities for working with weak references, finalisers, and factory
-- pools.
module Graphics.QML.Objects.Weak (
    -- * Weak Object References
    WeakObjRef,
    toWeakObjRef,
    fromWeakObjRef,

    -- * Object Finalisers
    ObjFinaliser,
    newObjFinaliser,
    addObjFinaliser,

    -- * Factory Pools
    FactoryPool,
    newFactoryPool,
    getPoolObject
) where

import Graphics.QML.Internal.BindObj
import Graphics.QML.Internal.Marshal
import Graphics.QML.Objects

import Control.Concurrent.MVar
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Typeable

-- | Represents a weak reference to a QML object which wraps the type @tt@.
--
-- Unlike ordinary strong references, a weak reference does not prevent the
-- QML garbage collector from collecting the underlying object. Weak references
-- can be used to monitor the life cycles of QML objects.
data WeakObjRef tt = WeakObjRef {
    objHndl :: HsQMLObjectHandle
}

-- | Converts a strong 'ObjRef' into a 'WeakObjRef'. 
toWeakObjRef :: (Typeable tt) => ObjRef tt -> IO (WeakObjRef tt)
toWeakObjRef obj = do
    hndl <- mToHndl obj
    hndl' <- copyObjectHandle hndl True
    return $ WeakObjRef hndl'

-- | Converts a 'WeakObjRef' into a strong 'ObjRef'.
--
-- If the underlying QML object has already been collected then the resulting
-- 'ObjRef' can be used to reincarnate it.
fromWeakObjRef :: (Typeable tt) => WeakObjRef tt -> IO (ObjRef tt)
fromWeakObjRef wobj = do
    hndl <- copyObjectHandle (objHndl wobj) False
    mFromHndl hndl

-- | Represents an object finaliser function for QML objects which wrap the
-- type @tt@.
data ObjFinaliser tt = ObjFinaliser HsQMLObjFinaliserHandle

-- | Create a new object finaliser from a finaliser function.
--
-- Note that at the time the finaliser is called the runtime will have already
-- comitted to collecting the underlying QML object. The 'ObjRef' passed into
-- the finaliser can be used to reincarnate the object, but this QML object
-- will have a distinct identity to the original.
newObjFinaliser :: (Typeable tt) => (ObjRef tt -> IO ()) -> IO (ObjFinaliser tt)
newObjFinaliser f = do
    fPtr <- marshalObjFinaliser $ \hPtr -> do
        hndl <- newObjectHandle hPtr
        obj <- mFromHndl hndl
        f obj
    final <- hsqmlCreateObjFinaliser fPtr
    return $ ObjFinaliser final

-- | Adds an object finaliser to an QML object.
--
-- The finaliser will be called no more than once for each time it was added to
-- an object. The timing of finaliser execution is subject to the combined
-- behaviour of the Haskell and QML garbage collectors. All outstanding
-- finalisers will be run when the QML engine is terminated provided that the
-- program does not prematurely exit.
addObjFinaliser :: (Typeable tt) => ObjFinaliser tt -> ObjRef tt -> IO ()
addObjFinaliser (ObjFinaliser final) obj = do
    hndl <- mToHndl obj
    hsqmlObjectAddFinaliser hndl final

-- | Represents an object factory which maintains a one-to-one mapping between
-- values of type @tt@ and QML object instances.
--
-- 'ObjRef's manufactured by the pool are cached using the wrapped type @tt@ as
-- the lookup key in an ordered map. The pool uses weak references to
-- automatically purge objects which no longer have any strong references
-- leading to them from either Haskell or QML code.

-- Hence, the pool guarantees that if QML code is using a pool object (e.g. as
-- a source for data binding) then the same object instance can be obtained
-- again from the pool. Conversely, if an object instance is no longer being
-- used then pool will not prevent it from being garbage collected.
data FactoryPool tt = FactoryPool {
    factory   :: tt -> IO (ObjRef tt),
    pool      :: MVar (Map tt (WeakObjRef tt)),
    finaliser :: ObjFinaliser tt
}

-- | Creates a new 'FactoryPool' using the supplied factory function.
newFactoryPool :: (Ord tt, Typeable tt) =>
    (tt -> IO (ObjRef tt)) -> IO (FactoryPool tt)
newFactoryPool factory = do
    pool <- newMVar Map.empty
    finaliser <- newObjFinaliser $ \obj ->
        modifyMVar_ pool (return . Map.delete (fromObjRef obj))
    return $ FactoryPool factory pool finaliser

-- | Return the pool's canonical QML object for a value of @tt@, either by
-- creating it or looking it up in the pool's cache of objects.
getPoolObject :: (Ord tt, Typeable tt) =>
    FactoryPool tt -> tt -> IO (ObjRef tt)
getPoolObject (FactoryPool factory pool finaliser) value =
    modifyMVar pool $ \map ->
        case Map.lookup value map of
            Just wkObj -> do
                obj <- fromWeakObjRef wkObj
                return (map, obj)
            Nothing  -> do
                obj <- factory value
                addObjFinaliser finaliser obj
                wkObj <- toWeakObjRef obj
                return (Map.insert value wkObj map, obj) 
