module REVC (
    reverseComplement
    ) where

-- [1] Simple implementation

type DNAString = String
type Nucleotide = Char

nucComplement:: Nucleotide -> Nucleotide
nucComplement n = 
    case n of
        'A' -> 'T'
        'T' -> 'A'
        'C' -> 'G'
        'G' -> 'C'
        _   -> n

reverseComplement:: DNAString -> DNAString
reverseComplement = map nucComplement . reverse
