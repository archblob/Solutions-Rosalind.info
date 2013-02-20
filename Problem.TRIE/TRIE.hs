module TRIE where

data Edge = Edge { p::Int , ch::Char, d::Trie } deriving Show

data Trie = Trie { v::Int, cl::[Edge] } deriving Show


sampleData::[String]
sampleData = ["ATAGA","ATC","GAT"]

empty::Trie
empty = Trie 1 []

lookupEdge::Char -> [Edge] -> Maybe Edge
lookupEdge c [] = Nothing
lookupEdge c ((Edge i c' t):es)
    | c == c' = Just $ Edge i c' t
    | otherwise = lookupEdge c es
