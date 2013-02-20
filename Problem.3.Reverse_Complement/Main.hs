module Main(main) where

import System.Environment(getArgs)

import ReverseComplement

main::IO ()
main = do
    [datasetF,resultF] <- getArgs
    dataset <- readFile datasetF
    let result = reverseComplement dataset
    writeFile resultF result
    print result

