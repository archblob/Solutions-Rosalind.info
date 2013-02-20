module ProteinTranslation where

import qualified Data.Map as M
import Data.Maybe

type RNAString = String
type Codon = String
type Protein = [AminoAcid]

data AminoAcid =
      F -- ^ Phenylalanine
    | L -- ^ Leucine
    | I -- ^ Isoleucine
    | M -- ^ Methionine
    | V -- ^ Valine
    | S -- ^ Serine
    | P -- ^ Proline
    | T -- ^ Threonine
    | A -- ^ Alanine
    | Y -- ^ Tyrosine
    | H -- ^ Histidine
    | Q -- ^ Glutamine
    | N -- ^ Asparagine
    | K -- ^ Lysine
    | D -- ^ Aspartic acid
    | E -- ^ Glutamic acid
    | C -- ^ Cysteine
    | W -- ^ Tryptophan
    | R -- ^ Arginine
    | G -- ^ Glycine
    | Stop deriving (Eq,Enum,Show)

codonDict:: M.Map Codon AminoAcid
codonDict = M.fromList [("UUU",F),("CUU",L),("AUU",I),("GUU",V),
                        ("UUC",F),("CUC",L),("AUC",I),("GUC",V),
                        ("UUA",L),("CUA",L),("AUA",I),("GUA",V),
                        ("UUG",L),("CUG",L),("AUG",M),("GUG",V),
                        ("UCU",S),("CCU",P),("ACU",T),("GCU",A),
                        ("UCC",S),("CCC",P),("ACC",T),("GCC",A),
                        ("UCA",S),("CCA",P),("ACA",T),("GCA",A),
                        ("UCG",S),("CCG",P),("ACG",T),("GCG",A),
                        ("UAU",Y),("CAU",H),("AAU",N),("GAU",D),
                        ("UAC",Y),("CAC",H),("AAC",N),("GAC",D),
                        ("UAA",Stop),("CAA",Q),("AAA",K),("GAA",E),
                        ("UAG",Stop),("CAG",Q),("AAG",K),("GAG",E),
                        ("UGU",C),("CGU",R),("AGU",S),("GGU",G),
                        ("UGC",C),("CGC",R),("AGC",S),("GGC",G),
                        ("UGA",Stop),("CGA",R),("AGA",R),("GGA",G),
                        ("UGG",W),("CGG",R),("AGG",R),("GGG",G)
                       ]

proteinToString::Protein -> String
proteinToString = foldl (\c v -> c ++ (show v)) ""

-- Unsafe partial function
fromRnaTranslate:: RNAString -> Protein
fromRnaTranslate [] = []
fromRnaTranslate rs = (fromJust (M.lookup codon codonDict)) : fromRnaTranslate (drop 3 rs)
    where codon = take 3 rs
