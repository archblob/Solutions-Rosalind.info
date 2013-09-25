module FIBD where

import qualified Data.Vector.Unboxed as V

fib::Int -> Int -> Int
fib dy g = V.sum $ fibd (V.fromList (1:replicate (dy-1) 0)) g

fibd::V.Vector Int -> Int -> V.Vector Int
fibd v 1 = v
fibd v g = fibd (V.imap shift v) (g-1)
    where
        vL = V.length v
        slc = V.sum $ V.slice 1 (vL-1) v
        shift 0 e = slc
        shift i e = v V.! (i-1)
