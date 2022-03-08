module Golf where

import           Data.Char
import           Data.List

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

stars :: [Int] -> [String]
stars xs
  | any (> 0) xs = map (\x -> if x > 0 then '*' else ' ') xs
  : stars (map (\x -> max (x - 1) 0) xs)
  | otherwise = []

histogram :: [Integer] -> String
histogram xs =
  unlines
    $  (reverse . stars . map (length . (\x -> filter (== x) xs)) $ [0 .. 9])
    ++ [replicate 10 '=', map intToDigit [0 .. 9]]
