module Ex1
  ( toDigits
  ) where

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n | n < 0     = []
              | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev
