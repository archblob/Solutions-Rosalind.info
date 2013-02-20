module REVP (
      restrictionSites412
    , tupleToString
    , DNAString
    ) where

type DNAString = String

reverseComplement:: DNAString -> DNAString
reverseComplement = foldl rev ""
    where
        rev c v =
            case v of
                'A' -> 'T':c
                'C' -> 'G':c
                'G' -> 'C':c
                'T' -> 'A':c
                _   -> ' ':c

restrictionSites412::DNAString -> [(Int,Int)]
restrictionSites412 l = go []  0 l
    where
        dim = length l
        go c _ [] = c
        go c p l = go (c ++ (computeC l p dim)) (p + 1) (drop 1 l)

computeC l p dim = go [] 4
    where
        go c 12 = c
        go c t
            | p + t > dim = c
            | str == reverseComplement str = go (c ++ [(p+1,t)]) (t + 1)
            | otherwise = go c (t + 1)
                where
                    str = take t l

tupleToString::[(Int,Int)] -> String
tupleToString = foldl toS ""
    where
        toS c (p,l) = c ++ (show p) ++ " " ++ (show l) ++ "\n"
