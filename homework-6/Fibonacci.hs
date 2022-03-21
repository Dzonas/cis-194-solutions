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
