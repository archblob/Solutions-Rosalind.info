module Main(main) where

import System.Environment(getArgs)
import qualified Data.ByteString.Char8 as B
import SPLC(spliceAndTranslate)

main::IO ()
main = do
    [datasetF,resultF] <- getArgs
    dataset <- B.readFile datasetF
    let result = spliceAndTranslate $ B.words dataset
    B.writeFile resultF result
    putStrLn "Done!"
