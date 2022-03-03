module Ex6
  ( hanoi4
  ) where

import           Ex5

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 n a b c d =
  hanoi4 (n - 2) a c b d
    ++ hanoi4 1       a d c b
    ++ hanoi4 1       a b c d
    ++ hanoi4 1       d b a c
    ++ hanoi4 (n - 2) c b a d
