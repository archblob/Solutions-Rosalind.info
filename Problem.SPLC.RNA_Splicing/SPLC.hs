module SPLC (
      composeDNAString
    , testString
    , dnaToProtein
    , spliceAndTranslate
    ) where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

testString::[B.ByteString]
testString = [B.pack "ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG",
              B.pack "ATCGGTCGAA",
              B.pack "ATCGGTCGAGCGTGT"]


composeDNAString::[B.ByteString] -> B.ByteString
composeDNAString (x:xs) = foldl comp x xs
    where
        comp nS e = B.append h (B.drop (B.length e) t)
            where
                (h,t) = B.breakSubstring e nS

dnaCodonTable:: M.Map B.ByteString B.ByteString
dnaCodonTable =
    M.fromList $ map (\(f,s) -> (B.pack f,B.pack s)) [("TTT","F"),("CTT","L"),("ATT","I"),
                ("GTT","V"),("TTC","F"),("CTC","L"),
                ("ATC","I"),("GTC","V"),("TTA","L"),      
                ("CTA","L"),("ATA","I"),("GTA","V"),
                ("TTG","L"),("CTG","L"),("ATG","M"),
                ("GTG","V"),("TCT","S"),("CCT","P"),
                ("ACT","T"),("GCT","A"),("TCC","S"),
                ("CCC","P"),("ACC","T"),("GCC","A"),
                ("TCA","S"),("CCA","P"),("ACA","T"),
                ("GCA","A"),("TCG","S"),("CCG","P"),
                ("ACG","T"),("GCG","A"),("TAT","Y"),
                ("CAT","H"),("AAT","N"),("GAT","D"),
                ("TAC","Y"),("CAC","H"),("AAC","N"),
                ("GAC","D"),("TAA","Stop"),("CAA","Q"),      
                ("AAA","K"),("GAA","E"),("TAG","Stop"),
                ("CAG","Q"),("AAG","K"),("GAG","E"),
                ("TGT","C"),("CGT","R"),("AGT","S"),
                ("GGT","G"),("TGC","C"),("CGC","R"),
                ("AGC","S"),("GGC","G"),("TGA","Stop"),
                ("CGA","R"),("AGA","R"),("GGA","G"),
                ("TGG","W"),("CGG","R"),("AGG","R"),
                ("GGG","G")]

dnaToProtein::B.ByteString -> B.ByteString
dnaToProtein s = B.concat $ go s []
    where
        go s c
            | B.null s = c
            | otherwise = go (B.drop 3 s) (c ++ [myLookup (B.take 3 s) dnaCodonTable])
        myLookup e m
            | Just x <- M.lookup e m = if x == B.pack "Stop" then B.empty else x
            | otherwise = B.append (B.pack "[->") e

spliceAndTranslate:: [B.ByteString] -> B.ByteString
spliceAndTranslate = dnaToProtein . composeDNAString
