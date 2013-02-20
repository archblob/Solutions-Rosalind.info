module Main(main) where

import AFRQ
import System.Environment(getArgs)

main::IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    rawDataset <- readFile datasetF
    let dataset = map read $ words rawDataset ::[Double]
    let result  = calcProbs dataset
    writeFile resultF $ showProbs result
