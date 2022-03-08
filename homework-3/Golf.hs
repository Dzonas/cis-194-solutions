module Golf where

skips :: [a] -> [[a]]
skips xs = map takeModN [1 .. (length xs)]
 where
  takeModN n = map snd . filter ((== 0) . (`mod` n) . fst) $ zip [1 ..] xs
