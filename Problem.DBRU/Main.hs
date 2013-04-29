module Main(main) where

import System.Environment(getArgs)
import DBRU

main:: IO ()
main = do
    (dF:rF:_) <- getArgs
    rawDataset <- readFile dF
    let dataset = lines rawDataset
    writeFile rF $ dbru dataset
