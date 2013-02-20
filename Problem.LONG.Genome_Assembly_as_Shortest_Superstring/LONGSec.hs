module LONGSec where

import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
import Data.Maybe
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graphviz
import Data.Graph.Inductive.Query.MaxFlow
import Data.Graph.Inductive.Query.SP
import qualified Data.Set as S

-- greedy algorithm

superstringGreedy:: V.Vector B.ByteString -> B.ByteString
superstringGreedy set = go set
    where
        stdL = B.length $ V.head set
        go inn
            | V.length inn == 1 = V.head inn
            | otherwise = go $! ((fst (newIn maxScore)) V.++ (V.drop 1 (snd (newIn maxScore))))
                where
                    maxScore = V.ifoldl' getMax ((Suffix,0),0) inn
                    getMax c i e
                        | i == 0      = c
                        | otherwise   = if (snd currentOverlap) > snd (fst c) then (currentOverlap,i) else c
                            where
                                currentOverlap = overlap (V.head inn) e stdL
                    newIn (ov,j) = V.splitAt j (V.update inn (V.singleton (0,(merge ov (V.head inn) (inn V.! j))))) 

data Overlap = Suffix | Prefix

overlap:: B.ByteString -> B.ByteString -> Int -> (Overlap,Int)
overlap s1 s2 n
    | s1s2 >= s2s1  = (Suffix,s1s2)
    | s1s2 < s2s1   = (Prefix,s2s1)
        where
            s1s2 = score' (B.drop ((B.length s1) - n) s1) s2
            s2s1 = score' s2 (B.take n s1)

merge:: (Overlap,Int) -> B.ByteString -> B.ByteString -> B.ByteString
merge (Suffix,n) s1 s2 = B.append s1 (B.drop n s2)
merge (Prefix,n) s1 s2 = B.append s2 (B.drop n s1)


score'::B.ByteString -> B.ByteString -> Int
score' s1 s2 = maxSuff anchors 0
    where
        anchors  = B.elemIndices (B.head s2) s1
        prefix n = B.take (s1L - n) s2
        s1L      = B.length s1
        maxSuff a c
            | null a = c
            | (B.isSuffixOf (prefix e) s1) = (s1L - e)
            | otherwise = maxSuff (drop 1 a) c
                where
                    e = head a

one(a,_,_) = a
two(_,a,_) = a
three(_,_,a) = a


testData :: V.Vector B.ByteString
testData = V.fromList $ map B.pack ["CCTGCCGGAA","AGACCTGCCG","GCCGGAATAC","ATTAGACCTG"]


score::B.ByteString -> B.ByteString -> Maybe Int
score s1 s2 = maxSuff anchors Nothing
    where
        anchors  = B.elemIndices (B.head s2) s1
        prefix n = B.take (s1L - n) s2
        s1L      = B.length s1
        maxSuff a c
            | null a = c
            | (B.isSuffixOf (prefix e) s1) = Just (s1L - e)
            | otherwise = maxSuff (drop 1 a) c
                where
                    e = head a

dnaGraph::V.Vector B.ByteString -> Gr B.ByteString Int
dnaGraph v = mkGraph nodeList edgeList
    where
        nodeList = zip [1..(V.length v)] (V.toList v) 
        edgeList = edgeList'' v

dnaGraphPos::V.Vector B.ByteString -> Gr Int Int
dnaGraphPos v = mkGraph nodeList edgeList
    where
        nodeList = zip [1..(V.length v)] [1..(V.length v)]
        edgeList = edgeList'' v

edgeList':: V.Vector B.ByteString -> [(Int,Int,Int)]
edgeList' v = V.ifoldl' extMap [] v
    where
        extMap c i e = c ++ (intMap i e 0 v [])
        intMap eI e iI vc ac
            | V.null vc = ac
            | eI == iI = intMap eI e (iI + 1) (V.drop 1 vc) ac
            | otherwise = addEdge
                where
                    edgeWeigth = score e (V.head vc)
                    addEdge =
                        case edgeWeigth of
                            Just a  -> intMap eI e (iI + 1) (V.drop 1 vc) $! ac ++ [(eI + 1,iI+1,a)]
                            Nothing -> intMap eI e (iI + 1) (V.drop 1 vc) ac

maxPath:: Graph gr => gr Int Int -> Int -> Int -> Path
maxPath gr s e = mPHelp s e []
    where
        mPHelp s e p
            | s == e = p ++ [e]
            | otherwise = mPHelp (fst maxNode) e $! (p ++ [s])
                where
                    sLabel  = lab gr s
                    sEdges  = out gr s
                    maxNode = foldl (\c@(_,oV) (_,nxt,nValue) -> if nValue > oV then (nxt,nValue) else c) (0,0) sEdges

-- It does not work
startNode:: Graph gr => gr Int Int -> [Node]
startNode gr = ufold voidInward [] gr
    where
        voidInward cxt v
            | inn' cxt == [] = v ++ [(node' cxt)]
            | otherwise = v
        
-- It does not work
endNode:: Graph gr => gr Int Int -> [Node]
endNode gr = ufold voidOutward [] gr
    where
        voidOutward cxt v
            | out' cxt == [] = v ++ [(node' cxt)]
            | otherwise = v


edgeList'':: V.Vector B.ByteString -> [(Int,Int,Int)]
edgeList'' v = V.ifoldl' extMap [] v
    where
        extMap c i e = c ++ (intMap i e 0 v [])
        intMap eI e iI vc ac
            | V.null vc = ac
            | eI >= iI = intMap eI e (iI + 1) (V.drop 1 vc) ac
            | otherwise = addEdge
                where
                    edgeWeigth_sXsY = score e (V.head vc)
                    edgeWeigth_sYsX = score (V.head  vc) e
                    addEdge
                        | edgeWeigth_sXsY > edgeWeigth_sYsX = intMap eI e (iI + 1) (V.drop 1 vc) $! ac ++ [(eI + 1,iI+1,fromJust edgeWeigth_sXsY)]
                        | edgeWeigth_sXsY < edgeWeigth_sYsX = intMap eI e (iI + 1) (V.drop 1 vc) $! ac ++ [(iI + 1,eI+1,fromJust edgeWeigth_sYsX)]
                        | otherwise = if edgeWeigth_sXsY == Nothing then intMap eI e (iI + 1) (V.drop 1 vc) ac else intMap eI e (iI + 1) (V.drop 1 vc) $! ac ++ [(eI+1,iI+1,fromJust edgeWeigth_sXsY),(iI+1,eI+1,fromJust edgeWeigth_sYsX)]
