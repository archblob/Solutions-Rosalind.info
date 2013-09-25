{-# LANGUAGE  TypeOperators, DataKinds, BangPatterns, ExistentialQuantification #-}
-- Working file for ghci
module Main where 
-- import Data.Bits
-- import Data.Array.Repa
-- type NumList = forall a . (Num a ,Fractional a) => [a]
--
-- numList:: NumList
-- numList = [1,2,3]

-- fracList:: NumList
-- fracList = [1.3,1.7]


-- myArray = fromListUnboxed (Z :. (1234::Int)) ([1..1234]::[Int])

-- foo = foldP (\s i -> if i .&. (shift 1 (i `rem` 4)) /= 0 then s+1 else s) (0::Int) myArray

--properSum::Int -> Int
--properSum 0 = 0
--properSum 1 = 0
--properSum n
--    | even 2 = properWorker n 0 (n `div` 2)
--    | otherwise = properWorker n 0 ((n-1) `div` 2)

--properWorker:: Int -> Int -> Int -> Int
--properWorker _ s 0 = s
--properWorker n s c
--    | divProper n c  = properWorker n (s+c) (c-1)
--    | otherwise      = properWorker n s (c-1)

--divProper:: Int -> Int -> Bool
--divProper n d
--    | (d < n) && (n `rem` d == 0) = True
--    | otherwise = False

-- amicablePair:: Int -> Int -> Bool
-- amicablePair a b
--    | (a /= b) && (properSum a == b) && (properSum b == a) = True
--    | otherwise = False
--
-- isAmic n
--    | (psn < 10000) && amicablePair n psn = n 
--    | otherwise = 0
--        where psn = properSum n

-- sumOfAllAmicUnder n = foldl (+) 0 $ map isAmic [1..n]

-- main = print $ sumOfAllAmicUnder 10000

--import Data.Char
--import Data.List

--split f ls = (ls \\ sat):sat:[]
--    where sat = filter f ls
{--
import Data.List
import qualified Data.Map as M

{-
import Data.Semigroup

data Ballot c = Ballot { vote::M.Map c Int , valid::Bool } deriving (Show)

data Candidates = Alexandra | Flaviu | Luna deriving (Eq,Ord,Enum,Bounded,Show)

addPos::[c] -> [(c,Int)]
addPos ls = zip ls [0..end]
    where end = (length ls) - 1

scoreMap::(Eq c, Ord c, Enum c, Bounded c)
    => M.Map c Int
    -> (c,Int)
    -> M.Map c Int
scoreMap m (k,v) = M.insert k (up - v) m
        where up = fromEnum (maxBound `asTypeOf` k)

scoreBordaClassic:: (Ord c, Enum c, Eq c, Bounded c)
    =>[c]
    -> M.Map c Int
scoreBordaClassic = foldl scoreMap M.empty . addPos

bordaBallot:: (Eq c, Ord c, Enum c, Bounded c)
    => [c]
    -> Ballot c
bordaBallot ls
    | nub ls == ls  = Ballot map True
    | otherwise     = Ballot map False
        where map = scoreBordaClassic ls

-}

--instance Semigroup (Ballot c) where
--    (<>) l r = 

--}

{--
import Data.Array.IArray
import Data.Array.Unboxed
import Data.Ix

data Candidates = Alexandra | Flaviu | Luna deriving (Eq,Enum,Ord,Bounded,Show,Ix)

chL::[(Candidates,Int)]
chL = [(Alexandra,1),(Flaviu,0),(Luna,0)]
--}
{-
import  Data.Array.Repa

data C = Alex | Fla | Luna deriving (Eq,Enum,Ord,Bounded,Show)

data Ballot c = Ballot {
    vote::Array U (Z :. Int) Int
    } deriving Show

mkBallot::(Eq c ,Enum c,Ord c, Bounded c, Show c) => c -> Ballot c
mkBallot c = Ballot $ fromListUnboxed (Z :. max) (genSc c)
    where
        max = (fromEnum (maxBound `asTypeOf` c)) + 1

genSc::(Eq c,Enum c,Ord c,Bounded c,Show c) => c -> [Int]
genSc c = [ f x | x <- enumFrom (minBound `asTypeOf` c) , let f v = if x == c then 1 else 0]

showScore c b = index (vote b) (Z :. ((fromEnum c)))

-}

-- O alternativa este derivarea unei instante Shape pentru
-- forma (sh :. C) Astfel
{-
data B = Alexandra | Flaviu | Lna | Null deriving (Eq,Ord,Enum,Bounded,Show)

instance Shape sh => (sh :. B) where
    rank (sh :. _ ) = rank sh + 1
    zeroDim = zeroDim :. Null
    unitDim = unitDim :. (minBound::B)
    intersectDim (sh1 :. b1) (sh2 :. b2)
        = (intersectDim sh1 sh2 :. (min b1 b2)
    addDim (sh1 :. n1) (sh2 :. n2)
        = addDim sh1 sh2 :. n1
    size (shl :. n) = size shl * (fromEnum (maxBound::B))
    size (sh1 :. n)
-}
{-
import Data.Ix
import Data.Array.IArray
import Data.Array.Unboxed

data Candidates = Alexandra | Flaviu | Luna deriving (Eq,Enum,Ord,Bounded,Show,Ix)

data Ballot c  = Ballot {
    vote::(UArray c Int)
} deriving Show

mkBallot::(Eq c,Ord c,Enum c,Bounded c,Ix c)
    => c
    -> Ballot c
mkBallot c = Ballot $ array bounds [(c,1)]
    where bounds = (minBound `asTypeOf` c, maxBound `asTypeOf` c)
-}

--import Data.Array.IArray

--data Dim = A | B deriving (Eq,Ord,Enum,Bounded,Show,Ix)

--scores::[((Dim,Dim),Int)]
--scores = [((A,A),1),((A,B),2),((B,A),2),((B,B),1)]

--myArray::Array (Dim,Dim) Int
--myArray = array ((A,A),(B,B)) scores

{-
import Data.String
import Data.List

lo::Int
lo = 100

b::Int
b = 9999


ispali :: Int -> Bool
ispali n = go n 0
  where
    go 0 acc = acc == n
    go m acc = go (m `quot` 10) (acc * 10 + (m `rem` 10))

a::Int
a = 100

main:: IO ()
main = do
    print $ maxPal


comb':: Int -> Int
comb' !e = go 0 e e
    where
        go !v !el !up
            | up == lo = v
            | mult > v && ispali mult = go mult el (up-1)
            | otherwise = go v el (up-1)
                where mult = el * up

maxPal::Int
maxPal = go 0 b
    where
        go !v !up
            | up == lo = v
            | otherwise = go (comb' up) (up-1)


-- from stack

maxpal :: Int
maxpal = go 0 b
  where
    go mx i
        | i < a = mx
        | otherwise = go (inner mx b) (i-1)
            where
                inner m j
                    | j < a = m
                    | p > m && ispali p = inner p (j-1)
                    | otherwise = inner m (j-1)
                        where
                            p = i*j
-}

{-
ack !m !n
    | m == 0 = n+1
    | m > 0 && n == 0 = ack (m-1) 1
    | otherwise = ack (m-1) (ack m (n-1))
-}

--appendWrapper []     = [[]]
--appendWrapper (x:xs) = (append x xs) : appendWrapper xs

--subseq [] = []
--subseq l@(x:xs) = [l] ++ (sub l) ++ (subseq xs)

--sub [] = []
--sub l  = r : sub r
--    where r = init l

{-
import Data.Array
  ( Array
  , listArray
  , bounds
  , (!)
  )

-- |The solid data type of KMP table
data Table a = Table
  { alphabetTable :: Array Int a
  , jumpTable :: Array Int Int
  } deriving Show

-- |The 'build' function eats a pattern (list of some Eq) and generates a KMP table.
--
-- The time and space complexities are both O(length of the pattern)
build :: Eq a => [a] -> Table a
build pattern =
  let
    len = length pattern

    resTable = Table
      { alphabetTable = listArray (0,len-1) pattern
      , jumpTable = listArray (-1,len-1) $ (-2) : genJump (-1) 0
      }

    genJump _ 0 =
      let
        o = if 1 == len || alphabetTable resTable ! 0 /= alphabetTable resTable ! 1
          then -1
          else -2

        later = genJump (-1) 1
      in
        o : later

    genJump lastMPJump i =
      let
        ch = alphabetTable resTable ! i

        findJ j
          | j == -2 = -2
          | alphabetTable resTable ! (j+1) == ch = j
          | j == -1 = -2
          | otherwise = findJ (jumpTable resTable ! j)

        j = findJ lastMPJump

        o = if i+1 == len || alphabetTable resTable ! (i+1) /= alphabetTable resTable ! (j+2)
          then j+1
          else jumpTable resTable ! (j+1)

        later = genJump (j+1) (i+1)
      in o : later

  in
    resTable

-}

{-
type DNAString = String

testString::DNAString
testString = "TTACTCTCTCGCGAACTATTGCGACCTCTGCGGCGCGCTGACTATTCCGACGATTTACGCAAAGAATGAGATATTCACTACGGGTTACCTTGGACACTATACCATGCCACCCCTGAGACGTGATTATGGTGCTTTGCGCACACGCTAATTAGCCATGTTGAGGTCACGAGGTCCCCAATAGAGGTCCTCCGCGGTAATCTTGCATGTCACGTTTGCGGTTCAGTTCGACCTCCGGTCCAGCTGGTACCCTAGGTCAGCGGCCGTATCAGCGAGGGAATTCCCTCGGCAGAGAAGGGGAATAACGGCCTAACTCGGGCGGGTAAGACTATGATTTACCAGCTCAAAACAATTCGACATGCAGTTTCAGTCTTAGGCTCTGTTGAGAAGGGGCGCATGCTAAAGGCTGGTCTGCTGTAGGGATTTAACATAGCAACGCGATTCGCGGGTCCCGTGAAGAGGGCTACTATCTATAATATCATCGTACCTGTGTTTTCTTCTTTGTCAATTTTACTAGCGCGTAAAGAACTCTTATATGAGTCATAGTTAAGCCGCAGATAGTGCCGAGTACCTTTGGTCTCGGGCACGAGCACCCGTGAACGGGACCAGAGGACACCCCAGGCTGAGTTCCGGTATCGTAATTCCTTCGAGTTAAGGCATCGAAATGTCCGGGGCGCACAATGGGACAGTAACCGATCAAACAAAAATCAAGTCAGGTGCAATGGAACTTCTTGATGATCGTACGAACCTGTAAAGAAGCTAAGGAGGTAACATGGGGTGAGCGAATTGATGTCGCCGAATAATTCTCGGTCGTTCTAGATAACATGGAGAAACTGCAAGTGTAACGGATGAAGAGGCTATTACGTTTGCCGCACTTCTACAGCTAGTCCGGTCATATGCCGATCCTTCATAACCGTGCTACGTGGCACATATAACAGATGCGATCGCGACTAGGGAGGGTGCAAAAGAG"

reverseComplement:: DNAString -> DNAString
reverseComplement = foldl rev ""
    where
        rev c v =
            case v of
                'A' -> 'T':c
                'C' -> 'G':c
                'G' -> 'C':c
                'T' -> 'A':c
                _   -> ' ':c

restrictionSites412::DNAString -> [(Int,Int)]
restrictionSites412 l = go []  0 l
    where
        dim = length l
        go c _ [] = c
        go c p l = go (c ++ (computeC l p dim)) (p + 1) (drop 1 l)

computeC l p dim = go [] 4
    where
        go c 12 = c
        go c t
            | p + t > dim = c
            | str == reverseComplement str = go (c ++ [(p+1,t)]) (t + 1)
            | otherwise = go c (t + 1)
                where
                    str = take t l

tupleToString::[(Int,Int)] -> String
tupleToString = foldl toS ""
    where
        toS c (p,l) = c ++ (show p) ++ " " ++ (show l) ++ "\n"

-}
{-
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S
import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as V
import System.Environment(getArgs)
import Data.List 

dataset = do
    (datasetF:_) <- getArgs
    s <- B.readFile datasetF
    return $ B.lines s

main:: IO ()
main = do
    d <- dataset
    let result = longest $ lcssF d
    print result


-- PROBLEMA : lcss s t /= lcss t s si ar trebui sa fie
-- Se poate ajusta cu o lungime minim astfel incat sa
-- returneze doar segmentele comune cele mai mari
-- dar asta nu ma ajuta me mine cu nimic, pentru ca am nevoie si de
-- segmentele initiale daca am de cand sa compar mai mult de doua siruri
lcss s t = S.union (lcss' s t) (lcss' t s)

lcss'::B.ByteString -> B.ByteString -> S.Set B.ByteString
lcss' s t = lcssWorker 0 0 0 B.empty S.empty
    where
        tL = (B.length t)
        sL = (B.length s)
        lcssWorker  sI tI sqL sq sqSet
            | sI >= sL = S.insert sq sqSet
            | tI >= tL || (sI+sqL) >= sL = insertSq `seq` (lcssWorker (sI+1) 0 0 B.empty  insertSq)
            | currentS == currentT  = appendChar `seq` (lcssWorker sI (tI+1) (sqL+1) appendChar sqSet)
            | otherwise = (insertSq) `seq` (lcssWorker sI (tI+1) 0 B.empty insertSq)
                where
                    appendChar = B.snoc sq currentT
                    insertSq = if B.length sq >= 2 then S.insert sq sqSet else sqSet 
                    currentS = B.index s $! sI + sqL
                    currentT = B.index t tI

lcssR s t = R.fromUnboxed ((R.Z R.:. sL) R.:. tL) $ V.replicate (sL*tL) 0
    where
        sL = B.length s + 1
        tL = B.length t + 1

subseq::B.ByteString -> S.Set B.ByteString
subseq s = subseq' s S.empty
    where
        subseq' t st
            | B.null t  = st
            | otherwise = subseq' (B.drop 1 t) $! newSt
                where
                    newSt = foldl (\c e -> S.insert e c) st seq
                    seq   = drop 2 $! B.inits t

lcssF l = foldl' intersect (subseq (head l)) l
    where
        intersect c e = S.intersection c $! subseq e

longest = S.foldl' (\c e -> if B.length e >= B.length c then e else c) B.empty
-}

{-
oddSum a b = go a 0
    where
        go c s
            | c == b = if odd c then s + c else s
            | odd c = go (c+1) (s+c)
            | otherwise = go (c+1) s
-}

stripOdd::String -> IO ()
stripOdd f = do
    dataset <- readFile f 
    let result = unlines $ evenL $ lines dataset
    writeFile "resultEven" result

evenLines::[String] -> Int -> [String]
evenLines [] _ = []
evenLines (x:xs) c = if even c then x:evenLines xs (c+1) else evenLines xs (c+1)

evenL l = evenLines l 1
