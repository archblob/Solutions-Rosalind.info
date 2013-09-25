module MMCH where

data RNABase = RNABase { a::Integer, c::Integer, g::Integer, u::Integer } deriving Show

empty::RNABase
empty = RNABase 0 0 0 0

baseFreq::String -> RNABase
baseFreq = foldl countBase empty
    where
        countBase ac b
            | b == 'A' = ac { a = (a ac) + 1 }
            | b == 'C' = ac { c = (c ac) + 1 }
            | b == 'G' = ac { g = (g ac) + 1 }
            | otherwise = ac { u = (u ac) + 1 }

maxPerMatch::RNABase -> Integer
maxPerMatch (RNABase a c g u) = au * cg
    where
        cg = ways c g
        au = ways a u

ways::Integer -> Integer -> Integer
ways p q = product $ take (fromInteger (min p q)) [up,(up -1)..]
    where
        up = max p q

mmch::String -> Integer
mmch = maxPerMatch . baseFreq

-- Messed this up by using Int -- 
dataset::String
dataset = "AGCACUGCUUAAACUCACAACGCUUCACCUCUCCGUCUGAGGAUUGUACGUACCACUUCAGGACAAAAUAGUCUCGUACAGUGGCGCUCGA"


dataset2::String
dataset2 = "UAUCUACGCUUACCCUAUGACCGCUAGGGCUUACUAGUUAAAAGACUGGACGGUCCGCAGUCAACCUAGUCUGUUGAGCAACGCUAUGUA"
