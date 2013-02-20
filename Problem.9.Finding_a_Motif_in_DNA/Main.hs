module Main(main) where

import DNAMotif
import System.Environment(getArgs)

main::IO ()
main = do
    [datasetF,resultF] <- getArgs
    dataset <- readFile datasetF
    let daTa = lines dataset
    let str  = head daTa
    let mot  = head $ tail daTa
    let result = findMotif str mot
    writeFile resultF $ show result
    putStrLn "Done!"
