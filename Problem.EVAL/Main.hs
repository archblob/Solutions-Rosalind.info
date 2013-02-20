module Main(main) where

import System.Environment(getArgs)
import EVAL

main::IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    (n,s,ps) <- prepareData datasetF
    let result = computeExpected n s ps
    writeFile resultF $ stringify result
