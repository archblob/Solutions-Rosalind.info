module CONV where

import qualified Data.MultiSet as DM
import Numeric

type SpecSet = DM.MultiSet Rational

readSets::String -> IO (SpecSet ,SpecSet)
readSets path = do
    rawDataset <- readFile path

    let (rS1:rS2:_) = lines rawDataset
    let s1 = readFloatList rS1
    let s2 = readFloatList rS2
    
    return (DM.fromList s1,DM.fromList s2) 


readFloatList:: String -> [Rational]
readFloatList = map (\x -> fst (head ((readSigned readFloat) x))) . words

minkowskiDifference::SpecSet -> SpecSet -> SpecSet
minkowskiDifference s1 s2 = DM.unionsMap (diff s2) s1
    where
        diff s e = DM.map (\x -> e - x) s

maxMult::SpecSet -> (Int,Rational)
maxMult = DM.foldOccur maxMult' (0,(0::Rational))
    where
        maxMult' rN iN old@(iO,rO)
            | iN > iO = (iN,rN)
            | otherwise = old
