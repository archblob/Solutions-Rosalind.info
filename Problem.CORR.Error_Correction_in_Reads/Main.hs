module Main(main) where

import System.Environment(getArgs)
import CORR
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
import qualified Data.Map as M

main::IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    rawData <- B.readFile datasetF
    let dataset = V.fromList $ B.lines rawData
    let result  = errorCorrection dataset
    B.writeFile resultF $ corrMapToString result
    putStrLn "Done!"
