module Homework where

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' n = sum . filter even . takeWhile (/= 1) $ iterate
  (\x -> if even x then x `div` 2 else 3 * x + 1)
  n
