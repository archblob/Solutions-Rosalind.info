module Main(main) where

import System.Environment(getArgs)
import SIGN

main::IO ()
main = do
    [datasetF,resultF] <- getArgs
    dataset <- readFile datasetF
    let result = sign (read (take 1 dataset))
    writeFile resultF ( show (length result) ++ "\n" ++ listsToString result)
    putStrLn "Done!"
