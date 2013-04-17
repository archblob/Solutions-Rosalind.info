module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as B
import CTBL

main::IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    dataset <- B.readFile datasetF
    let stringTree = head $ B.lines dataset
    let answer = either (\e -> B.pack "Could not parse tree") (\ t -> B.pack (prettyCTBL ( ctbl t))) $ parseNWK stringTree
    B.writeFile resultF answer
    
