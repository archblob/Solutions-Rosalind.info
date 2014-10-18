module Maj where

maj :: [Int] -> Int
maj arr = verify $ fst $ foldl compMaj (0,0) arr
  where
    half = length arr `div` 2
    verify n
      | foldl' countOcc 0 arr > half = n
      | otherwise = -1
        where half = length arr `div` 2
              countOcc c e
                | e == n    = c + 1
                | otherwise = 0
    compMaj (maj,c) e
      | c == 0    = (e, 1)
      | maj == e  = (maj, c + 1)
      | otherwise = (maj, c - 1)
