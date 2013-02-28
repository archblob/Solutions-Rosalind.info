module TRIE where

data Trie a = 
    Trie {
        lbl::Maybe Int
      , edg::[(a,Trie a)]
    } 
    | EOW deriving Show

type AdjacencyList = [((Int,Int),Char)]
type EdgeList   = [(Char,Trie Char)]

empty::Trie a
empty = Trie (Just 1) []


insert'::Trie Char -> String -> Trie Char
insert' n "" = EOW
insert' EOW (x:xs)   = Trie Nothing [(x,insert' (Trie Nothing []) xs)]
insert' (Trie l el) (x:xs) = Trie l $ insertChar $ span (\(c,_) -> c /= x) el
    where
        insertChar (p,r)
            | null r    = (x, insert' (Trie Nothing []) xs) :p
            | otherwise = p ++ [(fst e, insert' (snd e) xs)] ++ (tail r)
                where
                    e = head r

adjacencyList::Trie Char -> AdjacencyList
adjacencyList t = adjacency 2 [] [(1,edg t)]

adjacency::Int -> AdjacencyList -> [(Int,EdgeList)] -> AdjacencyList
adjacency gI cL [] = cL
adjacency gI cL eL = adjacency gI0 (cL ++ cL0) eL0
    where
        (gI0,cL0,eL0) = foldl construct (gI,[],[]) eL

construct::(Int,AdjacencyList,[(Int,EdgeList)])
    -> (Int,EdgeList)
    -> (Int,AdjacencyList,[(Int,EdgeList)])
construct (gI,aL,eL) (pI,el') = foldl go (gI,aL,eL) el'
    where
        go (gI0,aL0,eL0) (x,tr)
            | EOW <- tr =  (gI0 + 1, ((pI,gI0),x):aL0 , eL0)
            | otherwise =  (gI0 + 1, ((pI,gI0),x):aL0, eL0 ++ [(gI0,edg tr)])

adjToString::AdjacencyList -> String
adjToString = unlines . map (\((p,c),e) -> unwords [show p, show c, [e]])
