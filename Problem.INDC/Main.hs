module Main(main) where

import INDC
import System.Environment(getArgs)

main::IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    rawDataset <- readFile datasetF
    let dataset = read $ head $ words rawDataset :: Integer
    writeResult resultF dataset
