module SPEC where

import Debug.Trace
import Data.Maybe

testData::[Float]
testData = [3524.8542,3710.9335,3841.974,3970.0326,4057.0646]

range (x,y) e
    | e >= x && e <= y = True
    | otherwise = False

monoisotopicMassTable::Float -> Char
monoisotopicMassTable e
    | range (71.0,71.9) e   = 'A'
    | range (103.0,103.9) e = 'C'
    | range (115.0,115.8) e = 'D' 
    | range (129.0,129.8) e = 'E' 
    | range (147.0,147.7) e = 'F' 
    | range (57.0,57.5) e   = 'G' 
    | range (137.0,137.5) e = 'H' 
    | range (113.0,113.5) e = 'I' 
    | range (128.070,128.099) e = 'K' 
    | range (113.0,113.5) e     = 'L' 
    | range (131.0,131.5) e     = 'M' 
    | range (156.0,156.5) e     = 'R' 
    | range (114.0,114.5) e     = 'N' 
    | range (97.0,97.5) e       = 'P' 
    | range (128.010,128.069) e = 'Q' 
    | range (87.0,87.5) e       = 'S' 
    | range (101.0,101.5) e     = 'T' 
    | range (99.0,99.5) e   = 'V' 
    | range (186.0,186.5) e = 'W' 
    | range (163.0,163.5) e = 'Y' 

spectrumToProtein::[Float] -> String
spectrumToProtein l = snd $ foldl addAmino ((head l),"") (tail l)
    where
        addAmino (f,s) e = (e, s ++ [monoisotopicMassTable (e-f)])
