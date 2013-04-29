module DBRU where

import qualified Data.Set as S

cmpBase::Char -> Char
cmpBase 'A' = 'T'
cmpBase 'T' = 'A'
cmpBase 'C' = 'G'
cmpBase 'G' = 'C'

stringify::(String,String) -> String
stringify (a,b) = "(" ++ a ++ ", " ++ b ++ ")"

testData::[String]
testData = ["TGAT","CATG","TCAT","ATGC","CATC","CATC"]

uSSc::[String] -> S.Set String
uSSc = S.fromList . compConcat
    where
        compConcat l = l ++ map revComp l
        revComp = map cmpBase . reverse

deBrujinGraph::S.Set String -> [(String,String)]
deBrujinGraph = S.foldr (\e c -> (init e, tail e):c) []

dbru::[String] -> String
dbru = unlines . map stringify . deBrujinGraph . uSSc
 
