module Main(
    main
    ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as V
import System.Environment(getArgs)
import FailureArray

main:: IO ()
main = do
    (datasetF:resultF:_) <-getArgs
    rawData <- B.readFile datasetF
    let dataset = head $ B.words rawData
    let result  = kmpTable dataset
    writeFile resultF $ unwords $ map (\x -> show x) $ V.toList result
    putStrLn $ show $ B.length dataset
    putStrLn "Done"
