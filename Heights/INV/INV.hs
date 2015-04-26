module Main where

import System.Environment (getArgs)
import System.IO (writeFile, readFile)

main :: IO ()
main = do
    (i:o:_) <- getArgs
    inStr   <- readFile i
    let arr = map read $ lines inStr :: [Integer]
    writeFile o $ show $ fst $ countInversions arr

countInversions :: [Integer] -> (Integer,[Integer])
countInversions []  = (0,[])
countInversions [a] = (0,[a])
countInversions [a,b]
  | a > b = (1,[b,a])
countInversions ls = (splits + lc + hc, nls)
    where
      mid  = fromIntegral (length ls) `div` 2
      (splits, nls) = merge low high
      (lc, low)     = countInversions (take mid ls)
      (hc, high)    = countInversions (drop mid ls)

merge :: [Integer] -> [Integer] -> (Integer, [Integer])
merge [] ys = (0, ys)
merge xs [] = (0, xs)
merge a@(x:xs) b@(y:ys)
  | x > y     = (fromIntegral (length a) + ocs, y : rtsplit)
  | otherwise = (oc, x : rst)
    where
        (ocs, rtsplit) = merge a ys
        (oc, rst) = merge xs b
