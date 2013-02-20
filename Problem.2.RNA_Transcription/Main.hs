module Main(main) where

import System.Environment (getArgs)

import RNATranscription

main::IO ()
main = do
    [datasetF,resultF] <- getArgs
    dataset <- readFile datasetF
    let result = transcribeDNAtoRNA dataset
    writeFile resultF result
    print result

