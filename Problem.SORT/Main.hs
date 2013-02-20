module Main(main) where

import qualified Data.Vector.Unboxed as V
import System.Environment(getArgs)
import SORT

main::IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    rawData <- readFile datasetF
    let (rS:rM:_) = lines rawData
    let s = V.fromList $ map read $ words rS ::V.Vector Int
    let m = V.fromList $ map read $ words rM ::V.Vector Int
    let (d,l) = reversalSort s m
    let result = [[show d]] ++ map (\(x,y) -> [show (min x y + 1),show (max x y +1)]) l
    writeFile resultF $ unlines $ map unwords result
