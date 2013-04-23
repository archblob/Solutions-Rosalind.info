module Main(main) where

import System.Environment(getArgs)
import FULL

main::IO ()
main = do
    (dF:_) <- getArgs
    rawDataset <- readFile dF
    let dataset = map read $ lines rawDataset :: [Double]
    let parentMass  = head dataset
    let spectraMass = tail dataset
    print $ full parentMass spectraMass
