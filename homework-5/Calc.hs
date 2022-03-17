module Calc where

import           ExprT

eval :: ExprT -> Integer
eval (Lit n         ) = n
eval (Add left right) = eval left + eval right
eval (Mul left right) = eval left * eval right
