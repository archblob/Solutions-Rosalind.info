module CORR (
      hammingDistance
    , reverseComplement
    , errorCorrection
    , corrMapToString
    ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Maybe

testData = V.fromList $ map B.pack ["TCATC","TTCAT","TCATC","TGAAA","GAGGA","TTTCA","ATCAA","TTGAT","TTTCC"]

reverseComplement:: B.ByteString -> B.ByteString
reverseComplement = B.foldl (\c e -> B.cons (complement e) c ) B.empty
    where
        complement e =
            case e of
                'A' -> 'T'
                'C' -> 'G'
                'G' -> 'C'
                'T' -> 'A'

hammingDistance:: B.ByteString -> B.ByteString -> Maybe Int
hammingDistance s t = if B.length s == B.length t then Just (go 0 s t) else Nothing
    where
        go d s t
            | B.null s = d
            | B.head s /= B.head t = go (d+1) (B.drop 1 s) (B.drop 1 t)
            | otherwise = go d (B.drop 1 s) (B.drop 1 t)

errorCorrection:: V.Vector B.ByteString -> M.Map B.ByteString B.ByteString
errorCorrection v = V.foldl matchIncorrect M.empty incorrect
    where
        (correct,incorrect) = V.partition (\x -> V.length (V.findIndices (\y -> x == y || reverseComplement x == y) v) >= 2) v
        matchIncorrect m e = M.insert e (findCorrection e) m
        findCorrection e
            | V.length crs == 1 = V.head crs
            | otherwise = reverseComplement (V.head crs')
                where
                    (crs,_)  = V.partition (\c -> hammingDistance e c == Just 1) correct
                    (crs',_) = V.partition (\c -> hammingDistance e (reverseComplement c) == Just 1) correct

corrMapToString:: M.Map B.ByteString B.ByteString -> B.ByteString
corrMapToString m = B.unlines $ map prt $ M.toList m
    where
        prt (k,v) = B.append (B.append k (B.pack "->")) v
