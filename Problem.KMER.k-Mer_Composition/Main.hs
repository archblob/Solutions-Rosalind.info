module Main(main) where

import System.Environment(getArgs)
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V

import KMER

main::IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    rawData <- B.readFile datasetF
    let dataset = B.concat $ tail $ B.words rawData
    let result  = kmerComposition dataset
    B.writeFile resultF (intVecToString result)
    putStrLn "Done!"
