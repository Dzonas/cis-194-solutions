module Ex3
  ( sumDigits
  ) where

import           Ex1                            ( toDigits )

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits
