module Main where

import System.Random
import System.Environment (getArgs)

-- with a k of 8
sampleDataset :: [Int]
sampleDataset = [2, 36, 5, 21, 8, 13, 11, 20, 5, 4, 1]

partition :: Int -> [Int] -> ((Int, [Int]), (Int, [Int]), (Int, [Int]))
partition p elms = partition' elms (0, []) (0, []) (0, [])
    where partition' [] lt eq gt = (lt, eq, gt)
          partition' (y : ys) lts@(slt, lt) ets@(set, et) gts@(sgt, gt)
                | y < p = partition' ys (slt + 1, (y : lt)) ets gts
                | y > p = partition' ys lts ets (sgt + 1, (y : gt))
                | otherwise = partition' ys lts (set + 1, (y : et)) gts

select :: StdGen -> Int -> Int -> [Int] -> Int
select gen k p es
    | k <= slt = let (i, gena') = randomR (0, slt - 1) gena in select gena' k (lt !! i) lt
    | k > slt + seq = let (i, genb') = randomR (0, sgt - 1) genb in select genb' (k - slt - seq) (gt !! i) gt
    | otherwise = p
        where ((slt, lt), (seq, eq), (sgt, gt)) = partition p es
              (gena, genb) = split gen

main :: IO ()
main = do
    [input, output] <- getArgs
    [n, list, k] <- fmap lines $ readFile input
    let intlist = map read $ words list
    stdgen <- getStdGen
    let (p, gen) = randomR (0, (read n) - 1) stdgen
    let result = select gen (read k) p intlist
    writeFile output $ show result