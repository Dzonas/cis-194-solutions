module Calc where

import           ExprT
import           Parser

eval :: ExprT -> Integer
eval (Lit n         ) = n
eval (Add left right) = eval left + eval right
eval (Mul left right) = eval left * eval right

evalStr :: String -> Maybe Integer
evalStr s = do
  x <- parseExp Lit Add Mul s
  return (eval x)
