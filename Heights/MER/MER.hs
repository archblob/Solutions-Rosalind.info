module MER where

merge :: [Int] -> [Int] -> [Int]
merge [] b = b
merge a [] = a
merge arr@(a:as) barr@(b:bs)
  | a < b = a : merge as barr
  | otherwise = b : merge arr bs
