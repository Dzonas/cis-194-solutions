module Ex2
  ( doubleEveryOther
  ) where

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith f [1 ..] . reverse
 where
  f i n = case i `mod` 2 of
    0 -> 2 * n
    _ -> n
