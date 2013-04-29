module Main(main) where

import Data.Maybe
import System.Environment(getArgs)
import PCOV

main::IO ()
main = do
    (dF:rF:_) <- getArgs
    rawDataset <- readFile dF
    let dataset = lines rawDataset
    writeFile rF $ maybe "" id $ pcov dataset
