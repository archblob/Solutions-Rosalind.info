module Main(main) where

import System.Environment(getArgs)
import ITWV

main::IO ()
main = do
    (dF:rF:_) <- getArgs
    rawDataset <- readFile dF
    let (master:patterns) = lines rawDataset
    let result = tableS $ itwvTable master patterns
    writeFile rF result
