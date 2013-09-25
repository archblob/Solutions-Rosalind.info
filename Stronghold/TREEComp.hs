module Main(main) where

import Data.Maybe
import System.Environment(getArgs)
import qualified Data.ByteString.Char8 as B

main::IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    rawData <- B.readFile datasetF
    let dataset = B.words rawData
    let nodes   = fst $ fromJust (B.readInt (head dataset))
    let result  = (nodes -1) -  ((length dataset) - 1)
    putStrLn (show result)
