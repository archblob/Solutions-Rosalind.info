module LGIS where

import qualified Data.Sequence as S
import qualified Data.Vector.Unboxed as V

-- Alternative using sequences
lis::V.Vector Int -> (Int, S.Seq Int)
lis = lis' (S.singleton 1) 1 1 1
    where
        lis' s i mI mL v
            | i >= dim = (mI,s)
            | otherwise = lis' (s S.|> lis0 ) (i+1) (if lis0 >= mL then i else mI) (if lis0 >= mL then lis0 else mL) v
                where
                    dim  = V.length v
                    lis0 = 1 + maxe (i-1) 0
                    maxe j m
                        | j < 0 = m
                        | v V.! i > v V.! j = maxe (j-1) (max m (S.index s j))
                        | otherwise = maxe (j-1) m

retSeq::V.Vector Int -> (Int,S.Seq Int) -> S.Seq Int
retSeq v (i,t) = retSeq' fe (S.singleton fe) (i-1) (ml0-1)
    where
        ml0 = S.index t i
        fe = v V.! i
        retSeq' l s j ml
            | j == 0 = s
            | sl == ml = retSeq' e ( e S.<| s) (j-1) (ml-1)
            | otherwise = retSeq' l s (j-1) ml
                where
                    sl = S.index t j
                    e = v V.! j

lgis::V.Vector Int -> S.Seq Int
lgis v = retSeq v $ lis v

lgisINV::V.Vector Int -> S.Seq Int
lgisINV v = S.reverse $ retSeq v' $ lis  v'
    where
        v' = V.reverse v

-- Same method using fold

lisF::V.Vector Int -> (Int,Int,S.Seq Int)
lisF v = V.ifoldl lis' (1,1,S.empty) v
    where
        lis' (mI,mL,s) i e = (if lis0 >= mL then i else mI,if lis0 >= mL then lis0 else mL, s S.|> lis0)
            where
                lis0 = 1 + maxe (i-1)  0
                maxe j m
                    | j < 0 = m
                    | e > v V.! j = maxe (j-1) (max m (S.index s j))
                    | otherwise = maxe (j-1) m

-- Fast (nlogn) method using binary search
-- TO DO
lisB::V.Vector Int -> (Int, S.Seq Int)
lisB = undefined

-- END

fs::(a,b,c) -> a
fs (a,_,_) = a

sn::(a,b,c) -> b
sn (_,b,_) = b

th::(a,b,c) -> c
th (_,_,c) = c
