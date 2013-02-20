{-# LANGUAGE TypeSynonymInstances , FlexibleInstances #-}
module CountingNucleotides (
    computeNucFreq
    ) where

-- [1] Simple implementation
type NucleotideFreq = (Int,Int,Int,Int)

neutralFreq::NucleotideFreq
neutralFreq = (0,0,0,0)

countNucleotide:: NucleotideFreq -> Char -> NucleotideFreq
countNucleotide (a,c,g,t) n =
    case n of
        'A' -> (a+1,c,g,t)
        'C' -> (a,c+1,g,t)
        'G' -> (a,c,g+1,t)
        'T' -> (a,c,g,t+1)
        _   -> (a,c,g,t)

computeNucFreq:: [Char] -> NucleotideFreq
computeNucFreq = foldl countNucleotide neutralFreq
