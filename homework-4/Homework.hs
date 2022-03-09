module Homework where

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' n = sum . filter even . takeWhile (/= 1) $ iterate
  (\x -> if even x then x `div` 2 else 3 * x + 1)
  n

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

treeHeight :: Tree a -> Integer
treeHeight Leaf           = 0
treeHeight (Node n _ _ _) = n

foldTree :: [a] -> Tree a
foldTree []       = Leaf
foldTree [x     ] = Node 0 Leaf x Leaf
foldTree (x : xs) = Node height left x right
 where
  height = max (treeHeight left) (treeHeight right) + 1
  left   = foldTree $ take n xs
  right  = foldTree $ drop n xs
  n      = length xs `div` 2
