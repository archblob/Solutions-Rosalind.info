module FailureArray where

import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Char8 as B

kmpTable:: B.ByteString -> V.Vector Int
kmpTable w = go w (V.replicate dim 0) 2 0
    where
        dim = B.length w
        go s t pos cnd
            | pos > dim = t
            | B.index s (pos-1) == B.index s  cnd =
                    go s (V.update t (V.singleton (pos-1,cnd+1) )) (pos + 1) (cnd + 1)
            | cnd > 0 = go s t pos (t V.! (cnd-1))
            | otherwise = go s t (pos + 1) cnd
