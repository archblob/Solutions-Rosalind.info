module Main(main) where

import WFMD
import System.Environment(getArgs)

main::IO ()
main = do
    (dF:rF:_) <- getArgs
    rawDataset <- readFile dF
    let dataset = readFe rawDataset
    let result  = (uncurry (uncurry feA)) dataset
    writeFile rF $ showFeArray result
