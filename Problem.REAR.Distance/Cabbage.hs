module Cabbage where

import qualified Data.Vector.Unboxed as V
import Data.Maybe

type Breakpoint = (Int,Int)

s1M = V.fromList [1,2,3,4,5,6,7,8,9,10] :: V.Vector Int
s1S = V.fromList [3,1,5,2,7,4,9,6,10,8] :: V.Vector Int

s2M = V.fromList [3,10,8,2,5,4,7,1,6,9] :: V.Vector Int
s2S = V.fromList [5,2,3,1,7,4,10,8,6,9] :: V.Vector Int

s3M = V.fromList [8,6,7,9,4,1,3,10,2,5] :: V.Vector Int
s3S = V.fromList [8,2,7,6,9,1,5,3,10,4] :: V.Vector Int

s4M = V.fromList [3,9,10,4,1,8,6,7,5,2] :: V.Vector Int
s4S = V.fromList [2,9,8,5,1,7,3,4,6,10] :: V.Vector Int

s5M = V.fromList [1,2,3,4,5,6,7,8,9,10] :: V.Vector Int
s5S = V.fromList [1,2,3,4,5,6,7,8,9,10] :: V.Vector Int

preprocess::V.Vector Int -> V.Vector Int -> V.Vector Int
preprocess m s = preprocess' s V.empty
    where
        preprocess' v n
            | V.null v = n
            | otherwise = preprocess' (V.drop 1 v) (V.snoc n nE)
            where
                e = V.head v
                nE = (fromJust (V.elemIndex e m)) + 1

padd:: V.Vector Int -> V.Vector Int
padd v = (flip V.snoc) ((V.length v) + 1) $ V.cons 0 v

breakpoints:: V.Vector Int -> [Breakpoint]
breakpoints v = breakpoints' v 0
    where
        breakpoints' v' i
            | V.null v' || V.length v' == 1 = []
            | abs ( e - e1) /= 1 = (i,i+1): breakpoints' t (i+1)
            | otherwise = breakpoints' t (i+1)
                where
                    e   = V.head v'
                    e1  = V.head t
                    t   = V.tail v'
