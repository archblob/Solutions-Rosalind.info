module Main(main) where

import System.Environment(getArgs)
import SharedMotif

main::IO ()
main = do
    [datasetF,resultF] <- getArgs
    preDataset <- readFile datasetF
    let dataset = words preDataset
    let result = maximum $ sharedMotifs dataset
    writeFile resultF result
