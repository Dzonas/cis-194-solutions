{-# LANGUAGE FlexibleInstances #-}
module Calc where

import           ExprT
import           Parser
import qualified StackVM                       as S

eval :: ExprT -> Integer
eval (Lit n         ) = n
eval (Add left right) = eval left + eval right
eval (Mul left right) = eval left * eval right

evalStr :: String -> Maybe Integer
evalStr s = do
  x <- parseExp Lit Add Mul s
  return (eval x)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit n = n > 0
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax left) (MinMax right) = MinMax (max left right)
  mul (MinMax left) (MinMax right) = MinMax (min left right)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 left) (Mod7 right) = Mod7 ((left + right) `mod` 7)
  mul (Mod7 left) (Mod7 right) = Mod7 ((left * right) `mod` 7)

instance Expr S.Program where
  lit n = [S.PushI n]
  add x y = x ++ y ++ [S.Add]
  mul x y = x ++ y ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = do
  parseExp lit add mul
