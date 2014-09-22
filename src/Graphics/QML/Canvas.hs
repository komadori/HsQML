{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies,
    FlexibleInstances
  #-}

-- | Facility for drawing graphics directly from Haskell into a QML element. 
module Graphics.QML.Canvas (
    OpenGLDelegate,
    OpenGLPaint,
    newOpenGLDelegate
) where

import Graphics.QML.Internal.BindCanvas
import Graphics.QML.Internal.BindPrim
import Graphics.QML.Internal.Marshal

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
data OpenGLPaint = OpenGLPaint

-- | Creates a new 'OpenGLDelegate' from a paint function.
newOpenGLDelegate :: (OpenGLPaint -> IO ()) -> IO OpenGLDelegate
newOpenGLDelegate paintFn = do
    hndl <- hsqmlCreateGldelegate
    hsqmlGldelegateSetup hndl (do
        let syncCb _ = return ()
            paintCb _ _ = paintFn OpenGLPaint
        return (syncCb, paintCb))
    return $ OpenGLDelegate hndl
