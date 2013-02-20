module Main(main) where

import System.Environment(getArgs)
import SPEC

main::IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    rawDataset <- readFile datasetF
    let dataset = map read  $ lines rawDataset :: [Float]
    let result  = spectrumToProtein dataset
    writeFile resultF result
