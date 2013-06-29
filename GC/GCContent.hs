module GCContent (
       gcOnFile
     , module RosalindFASTA
    ) where

import RosalindFASTA

type DNAString = String
type Nucleotide = Char
type GCfTotal  = (Double,Double)

neutralGCfTotal:: GCfTotal
neutralGCfTotal = (0.0,0.0)

countGC:: GCfTotal -> Nucleotide -> GCfTotal
countGC (gc,t) n =
    case n of
        'G' -> (gc+1,t+1)
        'C' -> (gc+1,t+1)
        'A' -> (gc,t+1)
        'T' -> (gc,t+1)
        _   -> (gc,t)

calcGCPercentage:: GCfTotal -> Double
calcGCPercentage (gc,t) = (gc / t) * 100

gcContent:: DNAString -> Double
gcContent = calcGCPercentage . foldl countGC neutralGCfTotal

gcOnFile::[RFASTA] -> [(String,Double)]
gcOnFile = map go 
    where
        go r = ((idR (rid r)), gcContent (dna (dnaS r))) 
