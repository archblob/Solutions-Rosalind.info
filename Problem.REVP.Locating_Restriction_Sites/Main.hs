module Main(main) where

import System.Environment(getArgs)
import REVP

main::IO ()
main = do
    [datasetF,resultF] <- getArgs
    rawData <- readFile datasetF
    let stData = head $ words rawData
    let result = tupleToString $ restrictionSites412 stData
    writeFile resultF result
    putStrLn "Done!"

