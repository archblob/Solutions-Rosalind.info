module REAR where

import qualified Data.Vector.Unboxed as V
import Data.Maybe
import Data.List

data ScoredSection = SS {
              fstZero::Int
            , score::Int 
            , section::(V.Vector (Int,Int))
            } deriving Show

instance Eq ScoredSection where
    (==) ss1 ss2 = (score ss1) == (score ss2)


diffSign:: Int -> Int -> Bool
diffSign a b
    | a >= 0 && b >= 0 = False
    | a < 0 && b < 0 = False
    | otherwise = True

preprocess::V.Vector Int -> V.Vector Int -> V.Vector Int
preprocess s t = V.imap annotateDir t
    where
        annotateDir c e = (masterPos - c)
            where
                masterPos = maybe 0 id (V.elemIndex e s)

reverseInterval::V.Vector Int -> V.Vector Int
reverseInterval v = rev 0 ((V.length v) -1) v
    where
        rev i j v
            | i >= j = v
            | otherwise = rev (i+1) (j-1) $! newV
                where
                    vI = v V.! i
                    vJ = v V.! j
                    newV = v V.// [(i,vJ + (j-i)),(j,(vI-(j-i)))]

zeroCount::V.Vector Int -> Int
zeroCount = V.foldl (\c e -> if e == 0 then c+1 else c) 0

firstZero::V.Vector (Int,Int) -> Int
firstZero v
    | V.null v = 999
    | snd (V.head v) == 0 = fst (V.head v)
    | otherwise = firstZero (V.drop 1 v)

tryRev:: Int -> V.Vector Int -> ScoredSection
tryRev i v = SS (firstZero sec) (zA - zB)  $! sec
    where
        vI = v V.! i
        zA = zeroCount newInterval
        zB = zeroCount section
        sec = (V.zip (V.fromList secInterval) newInterval)
        section = V.ifilter (\dx _ -> elem dx secInterval) v
        newInterval = reverseInterval section
        secInterval = [(min i (i+vI))..(max i (i+vI))]

----------- BREADTH FIRST APROACH ----------------------------------
sectionL::V.Vector Int -> [V.Vector (Int,Int)]
sectionL v = V.ifoldl try [] v
    where
        try c i e
            | e /= 0 && diffSign e endValue = (section (tryRev i v)) : c
            | otherwise = c
                where
                    endValue = v V.! (i + e)

reversalD:: V.Vector Int -> Int
reversalD v = reversal' [(v,sectionL v)] 1

reversal'::[(V.Vector Int,[V.Vector (Int,Int)])] -> Int -> Int 
reversal' l d
    | Nothing <- newL = d
    | otherwise = seq newL (reversal' (fromJust newL) (d+1))
        where
            newL = foldl expandL (Just []) l

--expandL::Maybe [(V.Vector Int,[V.Vector (Int,Int)])] -> (V.Vector Int,V.Vector (Int,Int)) -> Maybe [(V.Vector Int,[V.Vector (Int,Int)])]
expandL (Just c) (v,sL) = fmap (c++) $ foldl (look4Empty v) (Just []) sL
expandL (Nothing) _     = Nothing

look4Empty v cm e
    | Just c <- cm = if null nS then Nothing else Just ((nV,nS):c)
    | otherwise    = Nothing
        where
            nV = V.update v e
            nS = sectionL nV

-- Sorting by reversals test data

expandReversals (Right s) _ = Right s
expandReversals (Left c) (m,v,sL,pathL)
    | Left l <- expanded = Left $ c ++ l
    | otherwise = expanded
        where
            expanded = foldl (isEnd m v pathL) (Left []) sL

isEnd m v path cm (mS,vS)
    | Left c <- cm = if null newSections then (Right newPathList) else Left $! ((newM,newV,newSections,newPathList):c)
    | otherwise = cm
        where
            newV = V.update v vS
            newM = V.update m mS
            newPathList = path ++ [mS] {- Here -}
            newSections = sections newM newV

-- Breath first annotated with reversed interval

sections::V.Vector Int -> V.Vector Int -> [(V.Vector (Int,Int),V.Vector (Int,Int))]
sections m v = V.ifoldl try [] v
    where
        try c i e
            | e /= 0 && diffSign e endValue = reverseSection i v m :c
            | otherwise = c
                where
                    endValue = v V.! (i+e)

reverseSection i v m = (newR,newP)
    where
        vI = v V.! i
        pSection  = V.ifilter (\dx _ -> elem dx interval) v
        rSection  = V.ifilter (\dx _ -> elem dx interval) m
        newP      = V.zip (V.fromList interval) $ reverseInterval pSection
        newR      = V.zip (V.fromList interval) $ V.reverse rSection
        interval  = [(min i (i+vI))..(max i (i+vI))]

revSort s m = reversalSort [(s,v,sections s v,[])] 1
    where
        v = preprocess s m

reversalSort l d
    | Right r <- newL = (d,r)
    | otherwise = seq newL (reversalSort (fromLeft newL) (d+1))
        where
            newL = foldl expandReversals (Left []) l

fromLeft (Left a) = a
fromLeft _  = error "Read the label!"

printRevD::(Int,[V.Vector (Int,Int)]) -> IO ()
printRevD (d,l) = do
    print d
    mapM _(\x -> putStrLn ((show ((fst (V.head x))+1) ) ++ " " ++ (show ((fst (V.last x))+1) ))) l
