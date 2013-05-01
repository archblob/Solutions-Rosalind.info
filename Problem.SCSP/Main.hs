module Main(main) where

import Data.ByteString.Char8 (pack)
import System.Environment(getArgs)
import SCSP

main::IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "No arguments Passed"
        df:rf:_ -> computeAndWrite df rf


computeAndWrite:: String -> String -> IO ()
computeAndWrite df rf = do
        rawDataset <- readFile df
        let (sa:sb:_) = lines rawDataset
        writeFile rf $ scs (pack sa) (pack sb)
