module TEST where

import Prelude hiding (foldr)
import Data.SuffixTree

-- Edge = (Prefix a, STree a)
--
-- data STree a = 
-- Leaf
-- Node [Edge a]

sample::String
sample = "CATACATAC$"

-- foldr :: (Prefix a -> b -> b) -> b -> STree a -> b


--dnaTree::[Char] -> STree Char
dnaTree s = construct s

--substrings::STree Char -> [[Char]]
substrings st = foldr ((:) . prefix) [] st

--nRepeats::STree String -> Int -> [String] -> String
nRepeats t n ss = filter  (\e -> countRepeats e t >= n ) ss

