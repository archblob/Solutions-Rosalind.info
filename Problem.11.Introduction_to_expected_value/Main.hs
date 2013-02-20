module Main(main) where

import System.Environment (getArgs)
import ExpectedValue
import Text.Printf

main::IO ()
main = do
    [datasetF,resultF] <- getArgs
    raw_dataset <- readFile datasetF
    let dataset = words raw_dataset
    let m   = read $ head dataset :: Int
    let n   = read $ head $ tail dataset :: Int
    let gcs = map read (drop 2 dataset)
    let result = map (\gc -> expectedValue m n gc) gcs :: [Float]
    writeFile resultF (unwords (map (printf "%.3f") result))
    print "Done!"
