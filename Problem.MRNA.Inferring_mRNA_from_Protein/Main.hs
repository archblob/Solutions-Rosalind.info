module Main(main) where

import System.Environment(getArgs)
import Rosalindlib.MRNA

main::IO ()
main = do
    [dataF,resultF] <- getArgs
    fileData <- readFile dataF
    let sanData = head $ words fileData
    let result  = infer sanData
    writeFile resultF $ show result
    putStrLn "Done!"
