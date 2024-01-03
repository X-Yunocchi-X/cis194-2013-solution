{-# LANGUAGE TypeSynonymInstances #-}

import ExprT
import Parser
import StackVM
import qualified StackVM as StackExp

eval :: ExprT -> Integer
eval (ExprT.Lit int) = int
eval (ExprT.Add expl expr) = eval expl + eval expr
eval (ExprT.Mul expl expr) = eval expl * eval expr

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax t1) (MinMax t2) = MinMax $ max t1 t2
    mul (MinMax t1) (MinMax t2) = MinMax $ min t1 t2

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit t = Mod7 $ mod t 7
    add (Mod7 t1) (Mod7 t2) = Mod7 $ div (t1+t2) 7
    mul (Mod7 t1) (Mod7 t2) = Mod7 $ div (t1*t2) 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr Program where
    lit t = [StackExp.PushI t]
    add a b = a ++ b ++ [StackExp.Add]
    mul a b = a ++ b ++ [StackExp.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
