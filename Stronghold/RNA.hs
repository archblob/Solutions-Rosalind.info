module RNA (
   transcribeDNAtoRNA
 ) where

-- [1] Simple implementation

type DNAString = String
type RNAString = String

transcribeNuc:: Char -> Char
transcribeNuc n =
    case n of
      'T' -> 'U'
      _   ->  n

transcribeDNAtoRNA:: DNAString -> RNAString
transcribeDNAtoRNA = map transcribeNuc
