module Graphics.QML.Test.MayGen where

import Test.QuickCheck.Gen
import Control.Applicative
import Data.Maybe

-- I wanted so badly to write a Monad instance for MayGen, but actually
-- Applicative captures the legal operations perfectly.

newtype MayGen a = MayGen {mayGen :: Maybe (Gen a)}

instance Functor MayGen where
    fmap f (MayGen v) = MayGen $ (fmap . fmap) f v

instance Applicative MayGen where
    pure = MayGen . Just . return
    (MayGen mf) <*> (MayGen v) = MayGen $ liftA2 (<*>) mf v

noGen :: MayGen a
noGen = MayGen Nothing

fromGen :: Gen a -> MayGen a
fromGen = MayGen . Just

mayElements :: [a] -> MayGen a
mayElements [] = noGen
mayElements xs = fromGen $ elements xs

mayOneof :: [MayGen a] -> MayGen a
mayOneof xs =
    case mapMaybe mayGen xs of
        []  -> noGen
        xs' -> fromGen $ oneof xs'
