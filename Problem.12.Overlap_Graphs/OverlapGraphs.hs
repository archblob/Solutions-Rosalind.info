module OverlapGraphs (
    overlapGraph
    , overlapGraphs
    ) where

import Data.List
import RosalindFASTA

-- using isPrefixOf
-- using isSuffixOf
-- take n for On

overlapGraph:: [RFASTA] -> Int -> [(RosalindID,RosalindID)]
overlapGraph [] _   = []
overlapGraph (x:xs) n = go xs
    where
        suf = drop ((length (dna (dnaS x))) - n) (dna (dnaS x))
        go []   = []
        go (y:ys)
            | isPrefixOf suf (dna (dnaS y)) = (rid x, rid y):go ys
            | otherwise = go ys

overlapGraphs::[RFASTA] -> [(RosalindID,RosalindID)]
overlapGraphs l = go l
    where
        go [] = []
        go (x:xs) = (overlapGraph ([x] ++ (l \\ [x])) 3 ) ++ (go xs)
