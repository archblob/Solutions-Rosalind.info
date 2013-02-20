module SSEQ (
      testData
    , subseqPos
    ) where

import Data.Maybe
import qualified Data.ByteString.Char8 as B

testData::[B.ByteString]
testData = [B.pack "ACGTACGTGACG",B.pack "GTA"]

subseqPos:: B.ByteString -> B.ByteString -> [Int]
subseqPos s t = go s t 0  []
    where
        go s t i c
            | (B.null t) || (B.null s) = c
            | otherwise = go nS (B.drop 1 t) (i+eI) (c ++ [eI+i])
                where
                    e  = head $ B.unpack $ B.take 1 t
                    eI = maybe (-2) (\e -> e + 1) (B.elemIndex e s)
                    nS = B.drop ((abs eI)) s


intListToString::[Int] -> B.ByteString
intListToString = foldl (\c e -> B.append (B.append c (B.pack (show e))) (B.pack " ")) B.empty
