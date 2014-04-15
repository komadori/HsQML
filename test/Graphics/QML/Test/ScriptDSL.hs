{-# LANGUAGE FlexibleInstances #-}

module Graphics.QML.Test.ScriptDSL where

import Data.Char
import Data.Int
import Data.List
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
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
call _ _ = error "cannot call the context object"

binOp :: String -> Expr -> Expr -> Expr
binOp op (Expr lhs) (Expr rhs) = Expr (
    showChar '(' . lhs . showString op . rhs . showChar ')')
binOp _ _ _ = error "cannot operate on the context object"

eq :: Expr -> Expr -> Expr
eq = binOp " == "

neq :: Expr -> Expr -> Expr
neq = binOp " != "

eval :: Expr -> Prog
eval (Expr ex) = Prog (ex . showString ";\n") id
eval _ = error "cannot eval the context object"

set :: Expr -> Expr -> Prog
set (Expr lhs) (Expr rhs) =
    Prog (lhs . showString " = " . rhs . showString ";\n") id
set _ _ = error "cannot set the context object"

saveVar :: Int -> Expr -> Prog
saveVar v (Expr rhs) =
    Prog (showString "var x" . shows v . showString " = " .
        rhs . showString ";\n") id
saveVar _ _ = error "cannot save the context object"

assert :: Expr -> Prog
assert (Expr ex) =
    Prog (showString "if (!" . ex .
        showString ") {Qt.quit(); throw -1;}\n") id
assert _ = error "cannot assert the context object"

connect :: Expr -> Expr -> Prog
connect sig fn = eval $ sig `dot` "connect" `call` [fn]

disconnect :: Expr -> Expr -> Prog
disconnect sig fn = eval $ sig `dot` "disconnect" `call` [fn]

makeCont :: [String] -> Prog -> Prog -> Prog
makeCont args (Prog a1 b1) (Prog a2 b2) =
    Prog (showString "var cont = function(" . farg . showString ") {\n" . a2)
        (b2 . showString "};\n" . a1 . b1)
    where farg = foldr1 (.) $ (id:) $ intersperse (showChar ',') $
                     map showString args

contVar :: Expr
contVar = sym "cont"

callee :: Expr
callee = sym "arguments.callee"

end :: Prog
end = Prog (showString "Qt.quit();\n") id
