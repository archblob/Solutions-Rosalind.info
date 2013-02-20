module Main(main) where

import Data.String.Utils
import ProteinTranslation
import qualified Data.Map as M
import System.Environment(getArgs)

main::IO ()
main = do
    [datasetF,resultF] <- getArgs
    dataset <- readFile datasetF
    let result = proteinToString $ fromRnaTranslate $ rstrip dataset
    writeFile resultF result
    putStrLn "Done!"
