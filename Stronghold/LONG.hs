module LONG where

import qualified Data.Set as S
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B
import Debug.Trace
-- Aici parcurg toata lista, dar e bine sa ma opresc
-- atunci cand am gasit un suffix, pentru ca incepand de la
-- indexul cel mai mic, stiu ca e cel mai mare sufix.
-- Optimiseaza!
--mScore:: B.ByteString -> B.ByteString -> Maybe Int
mScore s1 s2 = foldl maxSuff Nothing anchors
    where
       anchors = B.elemIndices (B.head s2) s1
       prefix n = B.take (s1L - n) s2
       s1L  = B.length s1
       maxSuff c e =
        case (B.isSuffixOf (prefix e) s1) of
            True -> maybe (Just e) (\x -> if x > e then Just e else Just x) c
            False -> c

mScore' s1 s2 = maxSuff anchors Nothing
    where
        anchors   = B.elemIndices (B.head s2) s1
        prefix n  = B.take (s1L - n) s2
        s1L       = B.length s1
        maxSuff a c
            | null a = c
            | (B.isSuffixOf (prefix e) s1) = Just (s1L - e) 
            | otherwise = maxSuff (drop 1 a) c
                where
                    e = head a

{-# INLINE appendWithScore #-}
appendWithScore:: B.ByteString -> B.ByteString -> Maybe Int -> B.ByteString
appendWithScore s1 s2 Nothing  = B.append s1 s2
appendWithScore s1 s2 (Just n) = B.append s1 (B.drop n s2) -- din cauza dependentei de lungime s2, sirurile trebuie sa aiba aceeasi lungime ca sa functioneze acest append with score


annotate:: [B.ByteString] -> V.Vector (Int,B.ByteString)
annotate = V.imap (\i e -> (i+1,e)) . V.fromList

dirList::V.Vector (Int,B.ByteString) -> V.Vector ((Int,Int),Maybe Int)
dirList v = go v V.empty
    where
        go m c
            | V.null m = c
            | otherwise = go (V.drop 1 m) (c V.++ (newC (V.head m) (V.tail m)))

--directionList::V.Vector B.ByteString -> V.Vector ((Int,Int),Maybe Int)
--directionList v = dirMap v 0 V.empty


newC:: (Int,B.ByteString) -> V.Vector (Int,B.ByteString) -> V.Vector ((Int,Int),Maybe Int)
newC s vC = calcD V.empty vC
    where
        calcD c v
            | V.null v  = c
            | Nothing <- sXsY , Just a <- sYsX  = calcD  (V.snoc c ((fst e,fst s),sYsX)) (V.drop 1 v)
            | Just a  <- sXsY , Nothing <- sYsX = calcD  (V.snoc c ((fst s,fst e),sXsY)) (V.drop 1 v)
            | Just a <- sXsY  , Just b <- sYsX  = calcD  (V.snoc c (if a > b then ((fst s,fst e),sXsY) else ((fst e,fst s),sYsX))) (V.drop 1 v)
            | otherwise = calcD c (V.drop 1 v)
            where
                sXsY = mScore' (snd s) (snd e)
                sYsX = mScore' (snd e) (snd s)
                e    = V.head v


minScore:: V.Vector ((Int,Int),Maybe Int) -> M.Map Int (Int,Maybe Int)
minScore = V.foldl addMin M.empty
    where
        addMin m ((sX,sY),sc)    = M.insertWith minIns sX (sY,sc) m
        minIns nV oV = if (fromJust (snd nV)) > (fromJust (snd oV)) then (fst nV,snd nV) else oV 

addEdges':: V.Vector ((Int,Int),Maybe Int) -> V.Vector [(Int,Maybe Int)]
addEdges' v = V.foldl addMin (V.replicate newLength []) v
    where
        newLength = 1 + (V.foldl (\c ((e,_),_) -> max c e) 0 v)
        addMin c ((sX,sY),sc) = c V.// [(sX, old ++ [(sY,sc)] )]
            where
                old = c V.! sX


--startNode':: V.Vector [(Int,Maybe Int)] -> Int
startNode' v = S.difference (S.fromList [1..((V.length v)-1)]) (V.foldl' (\c e -> S.union c (foldl' (\c' e' -> S.insert (fst e') c' ) S.empty e) ) S.empty v)

superstring:: M.Map Int (Int,Maybe Int) -> V.Vector B.ByteString -> B.ByteString
superstring m v = go 1 Nothing B.empty
    where
        go i sc str
            | i <= 0 = str
            | otherwise = go (fst nE) (snd nE) $!(appendWithScore str (v V.! (i-1)) sc)
                where
                    nE =
                        case M.lookup i m of
                            Just x  -> x
                            Nothing -> (0,Nothing)

superstring2::V.Vector (Int,Maybe Int) -> V.Vector B.ByteString -> B.ByteString
superstring2 m v = go 1 Nothing B.empty
    where
        go i sc str
            | i <= 0 = str
            | otherwise = go (fst nE) (snd nE) $! (traceShow a a)
                where
                    a  = (appendWithScore str (v V.! (i-1)) sc)
                    nE =
                        case m V.!? i of
                            Just x -> x
                            Nothing -> (0,Nothing)


superstring':: V.Vector (Int,Maybe Int) -> V.Vector B.ByteString -> [B.ByteString]
superstring' m v = go 1 Nothing []
    where
        go i sc str
            | i <= 0 = str
            | otherwise = go (fst nE) (snd nE) $!(str ++ [(dropWithScore (v V.! (i-1)) sc)])
                where
                    nE =
                        case m V.!? i of
                            Just x  -> x
                            Nothing -> (0,Nothing) 

dropWithScore::B.ByteString -> Maybe Int -> B.ByteString
dropWithScore str Nothing  = str
dropWithScore str (Just n) = B.drop n str


testData = annotate [s1,s2,s3,s4]
s1 = B.pack "ATTAGACCTG"
s2 = B.pack "CCTGCCGGAA"
s3 = B.pack "AGACCTGCCG"
s4 = B.pack "GCCGGAATAC"
