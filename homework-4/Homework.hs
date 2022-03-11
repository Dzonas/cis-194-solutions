module Homework where

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' n = sum . filter even . takeWhile (/= 1) $ iterate
  (\x -> if even x then x `div` 2 else 3 * x + 1)
  n

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf           = 0
height (Node h _ _ _) = h

insert :: a -> Tree a -> Tree a
insert n Leaf = Node 0 Leaf n Leaf
insert n (Node h left x right)
  | leftH <= rightH = Node newH (insert n left) x right
  | otherwise       = Node newH left x (insert n right)
 where
  newH   = max leftH rightH + 1
  leftH  = height left
  rightH = height right

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
