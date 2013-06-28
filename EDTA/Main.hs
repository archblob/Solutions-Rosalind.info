module Main(main) where

import System.Environment(getArgs)
import EDTA

main::IO ()
main = do
    (dF:rF:_) <- getArgs
    rawDataset <- readFile dF
    let (s:t:_) = lines rawDataset
    let (n,(sa,ta)) = edta s t
    writeFile rF $ unlines [show n,sa,ta]
