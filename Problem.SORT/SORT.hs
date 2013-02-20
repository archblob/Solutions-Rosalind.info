{-# LANGUAGE BangPatterns #-}
module SORT where

import Data.Ix
import qualified Data.Vector.Unboxed as V
import Prelude hiding(length)
import Data.Maybe

-- Reverse sort, depth first search implementation
s'::V.Vector Int
s' = V.fromList [3,2,1,4,5,7,9,8,6,10]

m'::V.Vector Int
m' = V.fromList [10,4,3,1,7,2,8,5,6,9]

m1::V.Vector Int
m1 = V.fromList [3,9,10,4,1,8,6,7,5,2]

s1::V.Vector Int
s1 = V.fromList [2,9,8,5,1,7,3,4,6,10]

preprocess::V.Vector Int -> V.Vector Int -> V.Vector Int
preprocess s t = V.imap direction t
    where
        direction c e = (masterPos - c)
            where
                masterPos = fromMaybe 0 (V.elemIndex e s)
{-# INLINE reversals #-}
reversals::V.Vector Int -> [(Int,Int)]
reversals v = V.ifoldr rev [] v
    where
        rev i e c
            | e /=0 && opus e jE = (i,j):c
            | otherwise = c
                where
                    j = i + e
                    jE = v V.! j

opus::Int -> Int -> Bool
opus p q
    | p < 0 = q >= 0
    | otherwise = q < 0

reverseInterval::V.Vector Int -> (Int,Int) -> V.Vector Int
reverseInterval v (s0,j0) = V.imap computeValue v
    where
        s = min s0 j0
        j = max s0 j0
        computeValue i e = if inRange (s,j) i then atOI - (i - oi) else e
            where
                oi = j - (i - s) 
                atOI = v V.! oi

points i j = V.fromList $  pt j i
    where
        pt p q
         | p < i && q > j = []
         | otherwise = p - q : pt (p-1) (q+1)

reversalSort::V.Vector Int -> V.Vector Int -> (Int,[(Int,Int)])
reversalSort s m = sort [(v,[])] 0
    where
        v = preprocess m s

sort::[(V.Vector Int,[(Int,Int)])] -> Int -> (Int,[(Int,Int)])
sort !l !d
    | Left path <- expanded  = (d,path)
    | Right newL <- expanded = seq newL $ sort newL (d+1)
        where
            expanded = acummExpand' l $ Right []

---
acummExpand::[(V.Vector Int,[(Int,Int)])]
    -> [(V.Vector Int,[(Int,Int)])]
    -> Either [(Int,Int)] [(V.Vector Int,[(Int,Int)])]
acummExpand !l !c
    | null l = Right c
    | null sections = Left path
    | otherwise = seq newC $ acummExpand (drop 1 l) newC
        where
            (v,path) = head l
            sections = reversals v
            one  = expand v path sections
            newC = seq one $ c ++ one


expand::V.Vector Int -> [(Int,Int)] -> [(Int,Int)] -> [(V.Vector Int,[(Int,Int)])]
expand !v !path !sections = map update sections
    where
        update s = seq newElement $! newElement
            where
                newElement = (newV,newPath)
                newV = reverseInterval v s
                newPath = path ++ [s]
----

acummExpand' !l !c
    | null l = c
    | Left p <- c = Left p
    | Right cm <- c = acummExpand' (drop 1 l) $! fmap (++ cm) one
        where
            (v,path) = head l
            one      = expand' v path
            newC r   = fmap (++ r) c 


expand' !v !path
    | null sections = Left path
    | otherwise = Right $ map update sections
        where
            sections = reversals v
            update s = seq newElement $! newElement
                where
                    newElement = (newV,newPath)
                    newV = reverseInterval v s
                    newPath = path ++ [s]
