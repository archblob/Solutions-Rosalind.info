module PDST (
    distanceMatrix
    ) where

import qualified Data.ByteString.Lazy.Char8 as B

testData::[B.ByteString]
testData = [ B.pack "TTTCCATTTA",
             B.pack "GATTCATTTC",
             B.pack "TTTCCATTTT",
             B.pack "GTTCCATTTA" ]

-- This is acutally the hamming distance / length
-- String must be of equal length
p_distance::B.ByteString -> B.ByteString -> Double
p_distance s1 s2
    | s1 == s2 = 0.0
    | otherwise = go s1 s2 0 / (fromIntegral (B.length s1))
        where
            go s1 s2 c
                | B.null s1 && B.null s2 = c
                | B.head s1 /= B.head s2 = go (B.drop 1 s1) (B.drop 1 s2) $! c + 1
                | otherwise = go (B.drop 1 s1) (B.drop 1 s2) c

distanceMatrix::[B.ByteString] -> [[Double]]
distanceMatrix l = map (\x -> map (\e -> p_distance x e) l) l
