module Main where

import Data.Foldable
import Prelude hiding (foldr)
import System.Environment(getArgs)
import LGIS
import qualified Data.Vector.Unboxed as V
import qualified Data.Sequence as S

main::IO ()
main = do
    (dF:rF:_) <- getArgs
    rawData <- readFile dF
    let seq = V.fromList $ map read $ words $ head $ tail $ lines rawData :: V.Vector Int
    let mxS = lgis seq
    let mxI = lgisINV seq
    let result = unlines $ map seqToString [mxS,mxI]
    writeFile rF result


seqToString::S.Seq Int -> String
seqToString = unwords . map show . toList
