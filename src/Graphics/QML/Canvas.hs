{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies,
    FlexibleInstances
  #-}

-- | Facility for drawing graphics directly from Haskell into a QML element. 
module Graphics.QML.Canvas (
    OpenGLDelegate,
    newOpenGLDelegate,
    OpenGLPaint,
    initData
) where

import Graphics.QML.Internal.BindCanvas
import Graphics.QML.Internal.BindPrim
import Graphics.QML.Internal.Marshal

import Data.IORef
import Data.Maybe
import Data.Tagged
import Control.Monad.Trans.Maybe

-- | Delegate for painting OpenGL graphics.
newtype OpenGLDelegate = OpenGLDelegate HsQMLGLDelegateHandle

instance Marshal OpenGLDelegate where
    type MarshalMode OpenGLDelegate c d = ModeBidi c
    marshaller = Marshaller {
        mTypeCVal_ = Tagged tyJSValue,
        mFromCVal_ = jvalFromCVal,
        mToCVal_ = jvalToCVal,
        mWithCVal_ = jvalWithCVal,
        mFromJVal_ = \ptr -> MaybeT $ do
            hndl <- hsqmlCreateGldelegate
            hsqmlGldelegateFromJval hndl ptr
            return $ Just $ OpenGLDelegate hndl,
        mWithJVal_ = \(OpenGLDelegate hndl) f ->
            withJVal (flip hsqmlGldelegateToJval) hndl f,
        mFromHndl_ = unimplFromHndl,
        mToHndl_ = unimplToHndl}

-- | Encapsulates parameters for paint operations.
data OpenGLPaint i = OpenGLPaint {
    -- | Gets state from initialisation callback.
    initData :: i
}

newOpenGLCallbacks ::
    (IO i) -> (OpenGLPaint i -> IO ()) -> (i -> IO ()) -> CallbacksFactory
newOpenGLCallbacks initFn paintFn deinitFn = do
    iRef <- newIORef Nothing
    let initCb = do
            iVal <- initFn
            writeIORef iRef $ Just iVal
        deinitCb = do
            iVal <- readIORef iRef
            deinitFn $ fromJust iVal
        syncCb _ = return ()
        paintCb _ _ = do
            iVal <- readIORef iRef
            paintFn $ OpenGLPaint (fromJust iVal)
    return (initCb, deinitCb, syncCb, paintCb)

-- | Creates a new 'OpenGLDelegate' from a paint function.
newOpenGLDelegate ::
    (IO i) -> (OpenGLPaint i -> IO ()) -> (i -> IO ()) -> IO OpenGLDelegate
newOpenGLDelegate initFn paintFn deinitFn = do
    hndl <- hsqmlCreateGldelegate
    hsqmlGldelegateSetup hndl (newOpenGLCallbacks initFn paintFn deinitFn)
    return $ OpenGLDelegate hndl
