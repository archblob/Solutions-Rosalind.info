module CUNR where

fact::Integer -> Integer
fact n = product [1..n]

cunr::Integer -> Integer
cunr n = ubtCount n `rem` 1000000

-- Be careful, nLeaf-1 can equal 0, and the prodcut will pe 0
ubtCount::Integer -> Integer
ubtCount nLeaf = product deimpartit `div` impartitor
    where
        deimpartit = [(nLeaf-1)..(2*nLeaf - 4)]
        impartitor = 2^(nLeaf-2)
