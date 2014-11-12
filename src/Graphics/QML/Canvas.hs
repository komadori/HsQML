{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies,
    FlexibleInstances
  #-}

-- | Facility for drawing graphics directly from Haskell into a QML element. 
module Graphics.QML.Canvas (
    OpenGLDelegate,
    newOpenGLDelegate,
    OpenGLType (
        OpenGLDesktop,
        OpenGLES),
    OpenGLSetup,
    openGLType,
    openGLMajor,
    openGLMinor,
    OpenGLPaint,
    OpenGLPaint',
    setupData,
    modelData,
    matrixPtr,
    itemWidth,
    itemHeight
) where

import Graphics.QML.Internal.BindCanvas
import Graphics.QML.Internal.BindPrim
import Graphics.QML.Internal.Marshal
import Graphics.QML.Marshal

import Data.IORef
import Data.Maybe
import Data.Tagged
import Control.Monad.Trans.Maybe
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CFloat)

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

-- | Represents the type of an OpenGL context.
data OpenGLType
    -- | Desktop OpenGL context.
    = OpenGLDesktop
    -- | OpenGL ES context.
    | OpenGLES
    deriving (Eq, Show)

mapGLType :: HsQMLGLCanvasType -> OpenGLType
mapGLType HsqmlGlDesktop = OpenGLDesktop
mapGLType HsqmlGlEs      = OpenGLES

-- | Encapsulates parameters for OpenGL setup.
data OpenGLSetup = OpenGLSetup {
    -- | Type of OpenGL context.
    openGLType :: OpenGLType,
    -- | Major version number of OpenGL context.
    openGLMajor :: Int,
    -- | Minor version number of OpenGL context.
    openGLMinor :: Int
}

-- | Encapsulates parameters for OpenGL paint.
data OpenGLPaint s m = OpenGLPaint {
    -- | Gets the setup state.
    setupData  :: s,
    -- | Gets the active model.
    modelData :: m,
    -- | Pointer to a 4 by 4 matrix which transform coordinates in the range
    -- (-1, -1) to (1, 1) on to the target rectangle in the scene.
    matrixPtr :: Ptr CFloat,
    -- | Width of the canvas item in its local coordinate system.
    itemWidth :: Float,
    -- | Height of the canvas item in its local coordinate system.
    itemHeight :: Float
}

-- | Specialised version of `OpenGLPaint` with no model.
type OpenGLPaint' s = OpenGLPaint s Ignored

newOpenGLCallbacks :: (Marshal m, CanGetFrom m ~ Yes) =>
    (OpenGLSetup -> IO i) -> (OpenGLPaint i m -> IO ()) -> (i -> IO ()) ->
    CallbacksFactory
newOpenGLCallbacks setupFn paintFn cleanupFn = do
    iRef <- newIORef Nothing
    mRef <- newIORef Nothing
    let setupCb ctype major minor = do
            iVal <- setupFn $ OpenGLSetup
                (mapGLType $ cIntToEnum ctype)
                (fromIntegral major) (fromIntegral minor)
            writeIORef iRef $ Just iVal
        cleanupCb = do
            iVal <- readIORef iRef
            cleanupFn $ fromJust iVal
        syncCb ptr = do
             mVal <- runMaybeT $ mFromJVal ptr
             writeIORef mRef mVal
             return $ if isJust mVal then 1 else 0
        paintCb mPtr w h = do
            iVal <- readIORef iRef
            mVal <- readIORef mRef
            paintFn $ OpenGLPaint
                (fromJust iVal) (fromJust mVal)
                mPtr (realToFrac w) (realToFrac h)
    return (setupCb, cleanupCb, syncCb, paintCb)

-- | Creates a new 'OpenGLDelegate' from setup, paint, and cleanup functions.
newOpenGLDelegate :: (Marshal m, CanGetFrom m ~ Yes) =>
    (OpenGLSetup -> IO i) -> (OpenGLPaint i m -> IO ()) -> (i -> IO ()) ->
    IO OpenGLDelegate
newOpenGLDelegate setupFn paintFn cleanupFn = do
    hndl <- hsqmlCreateGldelegate
    hsqmlGldelegateSetup hndl (newOpenGLCallbacks setupFn paintFn cleanupFn)
    return $ OpenGLDelegate hndl
