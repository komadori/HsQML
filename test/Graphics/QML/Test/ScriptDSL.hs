{-# LANGUAGE FlexibleInstances #-}

module Graphics.QML.Test.ScriptDSL where

import Data.Char
import Data.Int
import Data.List
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI
import Numeric

data Expr = Global | Expr {unExpr :: ShowS}

data Prog = Prog ShowS ShowS

instance Monoid Prog where
    mempty = Prog id id
    mappend (Prog a1 b1) (Prog a2 b2) = Prog (a1 . a2) (b2 . b1)

showProg :: Prog -> ShowS
showProg (Prog a b) = a . b

class Literal a where
    literal :: a -> Expr

instance Literal Int where
    literal x = Expr $ shows x

instance Literal Int32 where
    literal x = Expr $ shows x

instance Literal Double where
    literal x | isNaN x                 = Expr $ showString "(0/0)"
              | isInfinite x && (x < 0) = Expr $ showString "(-1/0)"
              | isInfinite x            = Expr $ showString "(1/0)"
              | isNegativeZero x        = Expr $ showString "-0"
              | otherwise               = Expr $ shows x

instance Literal [Char] where
    literal [] = Expr $ showString "\"\""
    literal cs =
        Expr (showChar '"' . (foldr1 (.) $ map f cs) . showChar '"')
        where f '\"' = showString "\\\""
              f '\\' = showString "\\\\"
              f c | ord c < 32 = hexEsc c
                  | ord c > 127 = hexEsc c
                  | otherwise  = showChar c
              hexEsc c = let h = showHex (ord c)
                         in showString "\\u" .  showString (
                                replicate (4 - (length $ h "")) '0') . h

instance Literal Text where
    literal = literal . T.unpack

instance Literal URI where
    literal = literal . ($ "") . uriToString id

var :: Int -> Expr
var 0 = Global
var n = Expr (showChar 'x' . shows n)

sym :: String -> Expr
sym name = Expr $ showString name

dot :: Expr -> String -> Expr
dot Global     m = Expr $ showString m
dot (Expr lhs) m = Expr (lhs . showChar '.' . showString m)

call :: Expr -> [Expr] -> Expr
call (Expr f) ps = Expr (
    f . showChar '(' . (
        foldr1 (.) $ (id:) $ intersperse (showChar ',') $ map unExpr ps) .
    showChar ')')

binOp :: String -> Expr -> Expr -> Expr
binOp op (Expr lhs) (Expr rhs) = Expr (
    showChar '(' . lhs . showString op . rhs . showChar ')')

eq :: Expr -> Expr -> Expr
eq = binOp " == "

neq :: Expr -> Expr -> Expr
neq = binOp " != "

eval :: Expr -> Prog
eval (Expr ex) = Prog (ex . showString ";\n") id

set :: Expr -> Expr -> Prog
set (Expr lhs) (Expr rhs) =
    Prog (lhs . showString " = " . rhs . showString ";\n") id

saveVar :: Int -> Expr -> Prog
saveVar var (Expr rhs) =
    Prog (showString "var x" . shows var . showString " = " .
        rhs . showString ";\n") id

assert :: Expr -> Prog
assert (Expr ex) =
    Prog (showString "if (!" . ex .
        showString ") {window.close(); throw -1;}\n") id
