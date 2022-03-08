module Golf where

skips :: [a] -> [[a]]
skips xs = map takeModN [1 .. (length xs)]
 where
  takeModN n = map snd . filter ((== 0) . (`mod` n) . fst) $ zip [1 ..] xs

localMaxima :: [Integer] -> [Integer]
localMaxima xs =
  map (\(_, b, _) -> b) . filter (\(a, b, c) -> b > a && b > c) $ zip3
    xs
    (drop 1 xs)
    (drop 2 xs)
