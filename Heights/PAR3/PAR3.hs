module Main where

import System.Environment (getArgs)

sampleDataset :: [Int]
sampleDataset = [4, 5, 6, 4, 1, 2, 5, 7, 4]

partition :: [Int] -> [Int]
partition (x : xs) = partition' xs [] [] []
    where partition' [] a b c = a ++ (x : b) ++ c
          partition' (y : ys) a b c
                | y < x = partition' ys (y : a) b c
                | y > x = partition' ys a b (y : c)
                | otherwise = partition' ys a (y : b) c

partition2 :: [Int] -> [Int]
partition2 [] = []
partition2 (x : xs) = partition2' xs [x] []
    where partition2' [] [] [] = []
          partition2' [] [] gt = gt
          partition2' [] (e : es) gt = e : partition2' [] es gt
          partition2' (y : ys) et  gt
                | y < x = y : partition2' ys et gt
                | y > x = partition2' ys et (y : gt)
                | otherwise = partition2' ys (y : et) gt

main :: IO ()
main = do
    [input, output] <- getArgs
    (head : r : []) <- fmap lines $ readFile input
    let dataset = map read $ words r
    writeFile output $ unwords $ map show $ partition2 dataset