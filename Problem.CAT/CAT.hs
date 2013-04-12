module CAT where

import qualified Data.ByteString.Char8 as B
import Data.MemoCombinators (memo2,integral)

cmp:: Char -> Char -> Integer
cmp 'A' 'A' = 0
cmp 'A' 'U' = 1
cmp 'A' 'C' = 0
cmp 'A' 'G' = 0
cmp 'U' 'A' = 1
cmp 'U' 'U' = 0
cmp 'U' 'C' = 0
cmp 'U' 'G' = 0
cmp 'C' 'A' = 0
cmp 'C' 'U' = 0
cmp 'C' 'C' = 0
cmp 'C' 'G' = 1
cmp 'G' 'A' = 0
cmp 'G' 'U' = 0
cmp 'G' 'C' = 1
cmp 'G' 'G' = 0
cmp  _   _  = 0


str::B.ByteString
str = B.pack "GCCCGGAUGCCCACGGCUCGGCCGGUCGCGAGUGCACCGCGCCGCGCGGGCCGCGCAACCGCGGCCGGUAUUACGUACAUGUUAUAGCUUAAGCGCGCGACUCGAAUGGCUCAUAAGCUUAUGCCGGGCGAUCGUACAGCUACGUGCGUAAGCCGUUAUUAGCUUAGAUCAAUAUCAUGCCGCGAUGGCAUCGCUAGAUACGAUUAUAUACAUGUCAGCUGUAAUCGAGCGCGCAUGUACGAGCUUUAACUAUAAUUAGCCGCGAU"



{-
cat:: B.ByteString -> Int
cat s = go (1,B.length s)
    where
        go (i,j)
            | n <= 0 = 1
            | n == 1 = cmp (B.index s (i-1)) (B.index s (j-1))
            | otherwise = sum [ (cmp (B.index s (i-1)) (B.index s (j'-1))) * go (i+1,j'-1) * go (j'+1,j) | k <- [1..(n `div` 2)] , let j'= (i-1) + 2*k ]
                where
                    n = (j - i) + 1
-}

cat::B.ByteString -> Integer
cat s = catalan (1::Integer) ( toInteger (B.length s)) `mod` 1000000
    where
        catalan = memo2 integral integral catalan'
            where
                catalan'::Integer -> Integer -> Integer
                catalan' i j
                    | n <= 0 = 1
                    | n == 1 = cmp (B.index s (fromInteger (i-1))) (B.index s (fromInteger (j-1)))
                    | otherwise = sum [ (cmp (B.index s (fromInteger (i-1))) (B.index s (fromInteger (j'-1)))) * catalan (i+1) (j' - 1) * catalan (j'+1) j | k <- [1..(n `div` 2)], let j' = (i-1) + 2*k]
                        where
                            n = (j-i) + 1
