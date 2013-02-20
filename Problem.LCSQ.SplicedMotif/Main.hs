module Main(main) where

import LCSQ
import System.Environment(getArgs)
import qualified Data.ByteString.Char8 as B

main::IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    rawData <- readFile datasetF
    let dataset = lines rawData
    let s = head dataset
    let t = head $ tail dataset
    let result = lcs'' s t
    writeFile resultF result
