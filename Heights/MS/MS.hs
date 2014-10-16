module MS where

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge a@(x:xs) b@(y:ys)
  | x < y = x : merge xs b
  | otherwise = y : merge a ys

mergeSort :: [Int] -> [Int]
mergeSort []  = []
mergeSort [a] = [a]
mergeSort [a,b]
  | a > b     = [b,a]
mergeSort ls = merge (mergeSort $ take middle ls) (mergeSort $ drop middle ls)
  where middle = length ls `div` 2
