module Main(main) where

import qualified Data.ByteString.Char8 as B
import System.Environment(getArgs)
import EDIT

main::IO ()
main = do
    (datasetF:_) <- getArgs
    rawData <- B.readFile datasetF
    let (s:t:_) = B.lines rawData
    let result = editDistance s t
    let result2 = levenshtein (B.unpack s) (B.unpack t)
    print result
    print result2
