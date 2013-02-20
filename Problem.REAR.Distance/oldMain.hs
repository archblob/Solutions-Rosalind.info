module Main(main) where

import System.Environment(getArgs)
import REAR
import Control.Applicative
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString as B

main::IO ()
main = do
    (datasetF:_) <- getArgs
    rawData <- readFile datasetF
    let dataset = group $ filter (\e -> not (null e)) $ lines rawData
    let result = map (\(m,s) -> reversalD (preprocess (toVec m) (toVec s))) dataset
    print result

group = gr []

gr c [] = c
gr c (x:y:xs) = gr (c++[(x,y)]) xs

toVec s = V.fromList $ intList s

intList:: String -> [Int]
intList s = read $ "[" ++ (map (\e -> if e == ' ' then ',' else e) s) ++ "]"
