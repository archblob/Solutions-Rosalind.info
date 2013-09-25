module ORF (
      orf
    , showOrfs
    , DNAString
    ) where

import qualified Data.Map as M
import Data.Maybe
import Data.List
testData1 = "AGCCATGTAGCTAACTCAGGTTACATGGGGATGACCCCGCGACTTGGATTAGAGTCTCTTTTGGAATAAGCCTGAATGATCCGAGTAGCATCTCAG"

testData2 = "ATGCTGAC"
{-
AGC 
CAT
GTA
GCT
AAC
TCA
GGT
TAC
ATG > Start > M
GGG > G
ATG > Start > M
ACC > T
CCG > P
CGA > R
CTT > P
GGA > G
TTA > L
GAG > E
TCT > S
CTT > L
TTG > L
GAA > E
TAA > Stop
GCC > A
TGA > Stop
ATG > Start > M
ATC > I
CGA > R
GTA > V
GCA > A
TCT > S
CAG > Q
-}


--showOrfs::[[[C]]] -> String
showOrfs = unlines . nub . concat

reverseComplement::DNAString -> DNAString
reverseComplement = foldl (\c e -> (complement e):c) ""
    where
        complement e =
            case e of
                'A' -> 'T'
                'C' -> 'G'
                'G' -> 'C'
                'T' -> 'A'

--orf::DNAString
orf s = map reaD [rf1,rf2,rf3,rf1',rf2',rf3']
    where
        rf1  = s
        rf2  = drop 1 s
        rf3  = drop 1 rf2
        rf1' = reverseComplement s
        rf2' = drop 1 rf1'
        rf3' = drop 1 rf2'

reaD s = go s []
    where
        go [] c = c
        go l c
            | codon == M = go (drop 3 l) (maybe c (\e -> c ++ [e]) (translateProtein l))
            | otherwise  = go (drop 3 l) c
                where
                    codon = maybe Unknown id (M.lookup (take 3 l) dnaCodonTable)

--translateProtein:: DNAString -> Maybe ProteinString
translateProtein = go []
    where
        go c dna
            | dna == []       = Nothing
            | look == Stop    = Just c
            | look /= Unknown = go (c ++ (show look)) (drop 3 dna)
            | otherwise       = Nothing
                where
                    codon = take 3 dna
                    look  = maybe Unknown id (M.lookup codon dnaCodonTable)

type DNAString = String
type ProteinString = [Aminoacid]

dnaCodonTable:: M.Map DNAString Aminoacid
dnaCodonTable =
    M.fromList [("GCT",A),("GCC",A),("GCA",A),("GCG",A),
                ("CGT",R),("CGC",R),("CGA",R),("CGG",R),("AGA",R),("AGG",R),
                ("AAT",N),("AAC",N),
                ("GAT",D),("GAC",D),
                ("TGT",C),("TGC",C),
                ("CAA",Q),("CAG",Q),
                ("GAA",E),("GAG",E),
                ("GGT",G),("GGC",G),("GGA",G),("GGG",G),
                ("CAT",H),("CAC",H),
                ("ATT",I),("ATC",I),("ATA",I),
                ("ATG",M),
                ("TTA",L),("TTG",L),("CTT",L),("CTC",L),("CTA",L),("CTG",L),
                ("AAA",K),("AAG",K),
                ("TTT",F),("TTC",F),
                ("CCT",P),("CCC",P),("CCA",P),("CCG",P),
                ("TCT",S),("TCC",S),("TCA",S),("TCG",S),("AGT",S),("AGC",S),
                ("ACT",T),("ACC",T),("ACA",T),("ACG",T),
                ("TGG",W),
                ("TAT",Y),("TAC",Y),
                ("GTT",V),("GTC",V),("GTA",V),("GTG",V),
                ("TAA",Stop),("TGA",Stop),("TAG",Stop)]

data Aminoacid =
      F
    | L
    | S
    | Y
    | C
    | W
    | P
    | H
    | Q
    | R
    | I
    | M
    | T
    | N
    | K
    | V
    | A
    | D
    | E
    | G
    | Unknown
    | Stop deriving (Eq,Show,Bounded)


