module Main(main) where

import System.Environment (getArgs)
import CountingNucleotides

main:: IO ()
main = do
    [datasetF,resultF] <- getArgs
    dataset <- readFile datasetF
    let result = computeNucFreq dataset
    writeFile resultF $ show result
    print result
