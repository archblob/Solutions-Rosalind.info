module MOTZ where

import Data.MemoCombinators
import qualified Data.ByteString.Char8 as B


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

dataset::B.ByteString
dataset = B.pack "GGAUGUGGGAAAUCUUGCAUAGGCCCCUACCUUGCGAGUGGCUGUCAGUGAUCGGUUGCUCAACUUUUAGAACGCUGAUUCCCGCACACCGUGUUUUGGGCGUGGGUACAGCUAGCCGCCGUGUCAAUGUCGUCACAAGGCCAAAGUGGACAGUGCCGGGUUGUGAGGACUACGCUUUCCCUCGGAUCUUCUAAUUAAUGACGUUUUGCACGGUGACACUUGUAUUUCGGUGUAGCAUAAUUCCCAGUAAAGCUGAAAUACACAUACCUAGGGUAACCUUUUAC"

testSeq::B.ByteString
testSeq = B.pack "AUAU"

mtz::B.ByteString -> Integer
mtz s = motzkin (1::Integer) ( toInteger (B.length s)) `mod` 1000000
    where
        motzkin = memo2 integral integral motzkin'
            where
                motzkin'::Integer -> Integer -> Integer
                motzkin' i j
                    | n <= 0 = 1
                    | n == 1 = 1
                    -- | n == 2 = motzkin (i+1) j + checkString i j
                    | otherwise = motzkin (i+1) j + sum [ checkString i j' * motzkin (i+1) (j'-1) * motzkin (j'+ 1) j | k <- [2..n], let j' = (i-1) + (k)]
                        where
                            n = (j-i) + 1
                            checkString a b = cmp (B.index s (fromInteger (a-1))) (B.index s (fromInteger (b-1)))
