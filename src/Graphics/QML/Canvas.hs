{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies,
    FlexibleInstances
  #-}

{-| Facility for drawing OpenGL graphics into the QML scenegraph.

To use this facility, you must place a @HaskellCanvas@ item into your
QML scene. This item can be imported from the @HsQML.Canvas 1.0@ module using
an @import@ statement in your QML script. It has several properties which can
be set from QML:

[@displayMode@] Specifies how the canvas is rendered with respect to the
rest of the scene. Possible values are:

    [@HaskellCanvas.Above@] The canvas shares a buffer with the scenegraph
    and is painted top of other items.
    [@HaskellCanvas.Inline@] The canvas has its own framebuffer object and the
    contents of this buffer are painted inline with other items (default).
    [@HaskellCanvas.Below@] The canvas shares a buffer with the scenegraph
    and is painted underneath other items.

[@canvasWidth@] Width of the framebuffer object in pixels. Defaults to the
item width.
[@canvasHeight@] Height of the framebuffer object in pixels. Defaults to the
item height.
[@delegate@] A marshalled 'OpenGLDelegate' value which specifies the Haskell
functions used to render the canvas.
[@model@] A value passed to delegate's paint function. The canvas is
repainted whenever this value changes.
[@status@] Either @HaskellCanvas.Okay@ or an error code (read only).

The @HsQML.Canvas 1.0@ module also contains another type of item called
@OpenGLConextControl@ which can be used to configure the OpenGL context used by
your windows. When placed inside a QML window, it has several properties which
when read return the current state of that window's OpenGL context, and when
written to cause the window's context to be reconfigured with a request for the
supplied setting. Note that as reconfiguring the context may cause a visible
window to dis- and re-appear, it's recommended to supply the desired settings
at startup or otherwise before the corresponding window is made visible.
Available properties are as below:

[@majorVersion@] Major component of the OpenGL version.
[@minorVersion@] Minor component of the OpenGL version.
[@contextType@] The type of OpenGL context. One of:
@OpenGLContextControl.UnknownType@, @OpenGLContextControl.OpenGL@, or
@OpenGLContextControl.OpenGLES@.
[@contextProfile@] The OpenGL context's profile. One of:
@OpenGLContextControl.NoProfile@, @OpenGLContextControl.CoreProfile@, or
@OpenGLContextControl.CompatibilityProfile@.
[@deprecatedFunctions@] True if deprecated functions are available.
[@depthBufferSize@] Depth buffer size in bits.
[@stencilBufferSize@] Stencil buffer size in bits.
[@when@] Any changes to the OpenGL context while this property is set to false
will be deferred until it is true again. The default value is true.
-}
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
import Graphics.QML.Internal.Types
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
        mFromJVal_ = \_ ptr -> MaybeT $ do
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
             mVal <- runMaybeT $ mFromJVal Strong ptr
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
