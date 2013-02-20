module Main(main) where

import System.Environment(getArgs)
import ProteinMass

main::IO ()
main = do
    [dataF,resultF] <- getArgs
    dataset <- readFile dataF
    let result = computePMass dataset
    writeFile resultF $ show result
    putStrLn "Done!"
