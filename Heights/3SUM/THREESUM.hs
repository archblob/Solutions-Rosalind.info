module Main where

import System.Environment (getArgs)
import qualified Data.Map as Map

sampleRow1 :: [Int]
sampleRow1 = [2, -3, 4, 10, 5]

sampleRow2 :: [Int]
sampleRow2 = [8, -6, 4, -2, -8]

sampleRow3 :: [Int]
sampleRow3 = [-5, 2, 3, 2, -4]

sampleRow4 :: [Int]
sampleRow4 = [2, 4, -5, 6, 8]

threesum :: [Int] -> Maybe (Int, Int, Int)
threesum lst = threesum' ilst ilst
    where ilst = zip [1,2..] lst
          set = Map.fromList $ zip lst [1,2..]
          threesum' [] _ = Nothing
          threesum' (y : ys) [] = threesum' ys ys 
          threesum' (y : ys) (x : xs)
            | Just i <- Map.lookup (negate sm) set, fst y < fst x, fst x < i = Just (fst y, fst x, i)
            | otherwise = threesum' (y : ys) xs
                where sm = snd y + snd x

main :: IO ()
main = do
    [input, output] <- getArgs
    (header : dataLines) <- fmap lines $ readFile input
    let dataset = map (map read . words) dataLines
    let result = map (\l -> case threesum l of
                                Just (a, b, c) -> unwords [show a, show b,show c]
                                Nothing -> "-1") dataset
    writeFile output $ unlines result
