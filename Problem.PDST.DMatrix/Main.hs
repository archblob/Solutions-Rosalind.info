module Main(main) where

import PDST
import Text.Printf
import Bio.Core.Sequence
import qualified Data.ByteString.Lazy.Char8 as B
import Bio.Sequence.Fasta
import System.Environment(getArgs)
import Data.List

main::IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    rawData <- readFasta datasetF
    let dataset = map (\(Seq l d x) -> unSD d) rawData
    let result  = distanceMatrix dataset
    let resultS = unlines $ map (\x -> unwords (map (\e -> printf "%.3f" (e::Double)) x)) result
    writeFile resultF resultS
    putStrLn "Done!"
