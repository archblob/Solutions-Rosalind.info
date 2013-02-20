module TRAN where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.List

sampleS1:: B.ByteString
sampleS1 = B.pack "GCAACGCACAACGAAAACCCTTAGGGACTGGATTATTTCGTGATCGTTGTAGTTATTGGAAGTACGGGCATCAACCCAGTT"

sampleS2::B.ByteString
sampleS2 = B.pack "TTATCTGACAAAGAAAGCCGTCAACGGCTGGATAATTTCGCGATCGTGCTGGTTACTGGCGGTACGAGTGTTCCTTTGGGT"

transition::Char -> Char -> Bool
transition c1 c2
    | c1 == 'A' && c2 == 'G' = True
    | c1 == 'G' && c2 == 'A' = True
    | c1 == 'C' && c2 == 'T' = True
    | c1 == 'T' && c2 == 'C' = True
    | otherwise = False

transversion::Char -> Char -> Bool
transversion c1 c2
    | c1 == 'A' && elem c2 ['C','T'] = True
    | c1 == 'C' && elem c2 ['A','G'] = True
    | c1 == 'T' && elem c2 ['A','G'] = True
    | c1 == 'G' && elem c2 ['C','T'] = True
    | otherwise = False

ratioTsTv:: B.ByteString -> B.ByteString -> (Int,Int)
ratioTsTv s1 s2 = go s1 s2 (0,0)
    where
        go s t c@(tr,tv)
            | B.null s  = c
            | (B.head s) == (B.head t) = go (B.drop 1 s) (B.drop 1 t) c
            | transition (B.head s) (B.head t) = go (B.drop 1 s) (B.drop 1 t) (tr+1,tv)
            | otherwise = go (B.drop 1 s) (B.drop 1 t) (tr,tv+1)
            
