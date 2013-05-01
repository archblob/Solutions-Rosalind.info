module SCSP where

import Prelude hiding (length)
import Data.ByteString.Char8 hiding (reverse,minimum)
import Data.Array hiding (index)

scs::ByteString -> ByteString -> String
scs sa sb = reverse $ traceback lenA lenB
    where
        lenA = length sa
        lenB = length sb
        a    = cons ' ' sa
        b    = cons ' ' sb
        costArray = listArray ((0,0),(lenA,lenB)) [costf i j | i <- [0..lenA] , j <- [0..lenB]]
        (@@)::Int -> Int -> Int
        (@@) i j  = costArray ! (i,j)
        costf i 0 = i
        costf 0 j = j
        costf i j
            | index a i /= index b j = minimum [(((i-1) @@ j) + 1),((i @@ (j-1)) + 1)]
            | otherwise = ((i-1) @@ (j-1)) + 1
        traceback i j = trace i j
            where
                trace 0 0 = []
                trace i 0 = index a i : trace (i-1) 0
                trace 0 j = index b j : trace 0 (j-1)
                trace i j
                    | i @@ (j-1)     == alfa - 1 = index b j : trace i (j-1)
                    | (i-1) @@ j     == alfa - 1 = index a i : trace (i-1) j
                    | (i-1) @@ (j-1) == alfa - 1 = index a i : trace (i-1) (j-1)
                        where
                            alfa = i @@ j
