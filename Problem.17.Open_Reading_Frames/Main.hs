module Main(main) where

import System.Environment(getArgs)
import OpenReadingFrames

main::IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    rawData <- readFile datasetF
    let dataset = head $ words rawData
    let result  = orf dataset
    writeFile resultF (showOrfs result)
    putStrLn "Done!"
