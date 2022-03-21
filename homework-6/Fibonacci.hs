module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- https://stackoverflow.com/a/21605696
fibo :: Integer -> Integer -> [Integer]
fibo a b = a : fibo b (a + b)

fibs2 :: [Integer]
fibs2 = fibo 0 1

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat n = Stream n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f z = Stream z (streamFromSeed f (f z))

nats :: Stream Integer
nats = streamFromSeed succ 0

divTest :: Integer -> Integer -> Integer
divTest k n | n `mod` (2 ^ k) == 0 = divTest (k + 1) n
            | otherwise            = k - 1

ruler :: Stream Integer
ruler = streamMap (divTest 0) $ streamFromSeed succ 1
