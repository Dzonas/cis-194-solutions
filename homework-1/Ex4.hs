module Ex4
  ( validate
  ) where

import           Ex1                            ( toDigits )
import           Ex2                            ( doubleEveryOther )
import           Ex3                            ( sumDigits )

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
