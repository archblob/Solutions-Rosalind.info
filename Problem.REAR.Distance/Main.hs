module Main(main) where

import System.Environment(getArgs)
import REAR
import qualified Data.Vector.Unboxed as V

main::IO ()
main = do
    (datasetF:_) <- getArgs
    rawData <- readFile datasetF
    let dataset = lines rawData
    let from = V.fromList $ map read $ words $ head dataset  :: V.Vector Int
    let to = V.fromList $ map read $ words $ head $ tail dataset :: V.Vector Int
    let result = revSort from to
    printRevD result
