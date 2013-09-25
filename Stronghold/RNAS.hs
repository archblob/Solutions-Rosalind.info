module RNAS where

import Data.MemoCombinators
import qualified Data.ByteString.Char8 as B

wbbl::Char -> Char -> Integer
wbbl 'A' 'C' = 0
wbbl 'C' 'A' = 0
wbbl 'G' 'A' = 0
wbbl 'A' 'G' = 0
wbbl 'C' 'U' = 0
wbbl 'U' 'C' = 0
wbbl b1 b2
    | b1 == b2 = 0
    | otherwise = 1

dataset::B.ByteString
dataset = B.pack "ACUCUAAUCUGCAGGGGUCUAUGUCGGCGUGUGCUGUAGACGGUCAUGCCAUUUCCUCGCUCCCAUCGUCGGAUAAUGUUGCGAUUAUAUUUCUGACGGAGAGUCUUCACUUUCCGGAUCCGACUGGACGCAAAAUAGAAUGACCAGGCAUUCCCGUGCGGAGUGUUUUUCAACCUUUGCCGAACGGGAAUCGACGG"


-- Should be 12
testSeq::B.ByteString
testSeq = B.pack "CGAUGCUAG"

-- Should be 284850219977421
testSeq2::B.ByteString
testSeq2 = B.pack "AUGCUAGUACGGAGCGAGUCUAGCGAGCGAUGUCGUGAGUACUAUAUAUGCGCAUAAGCCACGU"

rnas::B.ByteString -> Integer
rnas s = motzkin (1::Integer) ( toInteger (B.length s))
    where
        motzkin = memo2 integral integral motzkin'
            where
                motzkin'::Integer -> Integer -> Integer
                motzkin' i j
                    | n <= 0 = 1
                    | n == 1 = 1
                    | otherwise = motzkin (i+1) j + sum [ checkString i j' * motzkin (i+1) (j'-1) * motzkin (j'+ 1) j | k <- [2..n], let j' = (i-1) + (k)]
                        where
                            n = (j-i) + 1
                            checkString a b
                                | b < a + 4 = 0
                                | otherwise = wbbl (B.index s (fromInteger (a-1))) (B.index s (fromInteger (b-1)))
