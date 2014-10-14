module Main where

import Data.List
import System.Environment (getArgs)
import System.IO (writeFile, readFile)

main :: IO ()
main = do
    (f:o:_) <- getArgs
    fileS   <- readFile f
    let edgeList = tail $ map (map read . words) $ lines fileS :: [[Int]]
    let result   = deg edgeList
    writeFile o $ unwords $ map show result 

-- Simple and dirty using only prelude functions
deg :: [[Int]] -> [Int]
deg = map length . group . sort . concat
