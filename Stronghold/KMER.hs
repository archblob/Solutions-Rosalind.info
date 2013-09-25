module KMER (
      dna4MerVec
    , kmerComposition
    , testString
    , intVecToString
    ) where

import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B

testString:: B.ByteString
testString = B.pack "CTTCGAAAGTTTGGGCCGAGTCTTACAGTCGGTCTTGAAGCAAAGTAACGAACTCCACGGCCCTGACTACCGAACCAGTTGTGAGTACTCAACTGGGTGAGAGTGCAGTCCCTATTGAGTTTCCGAGACTCACCGGGATTTTCGATCCAGCCTCAGTCCAGTCTTGTGGCCAACTCACCAAATGACGTTGGAATATCCCTGTCTAGCTCACGCAGTACTTAGTAAGAGGTCGCTGCAGCGGGGCAAGGAGATCGGAAAATGTGCTCTATATGCGACTAAAGCTCCTAACTTACACGTAGACTTGCCCGTGTTAAAAACTCGGCTCACATGCTGTCTGCGGCTGGCTGTATACAGTATCTACCTAATACCCTTCAGTTCGCCGCACAAAAGCTGGGAGTTACCGCGGAAATCACAG"

fourMerVector aL = V.fromList [ B.pack (a:b:c:d:"") | a <- aL , b <- aL , c <- aL , d <- aL]

dna4MerVec::V.Vector B.ByteString
dna4MerVec = fourMerVector "ACGT"

kmerComposition:: B.ByteString -> V.Vector Int
kmerComposition dnaS = V.map comp dna4MerVec
    where
        comp e = length $ B.findSubstrings e dnaS

intVecToString::V.Vector Int -> B.ByteString
intVecToString = B.init . V.foldl sH B.empty
    where
        sH c v = B.append c (B.append (B.pack (show v)) (B.pack " "))
