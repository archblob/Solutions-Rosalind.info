module Main(main) where

import System.Environment(getArgs)
import LONGSec
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M

main:: IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    rawData <- B.readFile datasetF
    let dataset = B.words rawData
    let result  = superstringGreedy (V.fromList dataset)
    B.writeFile resultF  result
    putStrLn "Done!"
