module ConsensusProfile (
      listProfile
    , showProfile
    , Dataset
    , createDataset
    , consensus
    ) where

import qualified Data.List as L
import Data.Array.Repa

data Dataset = Dataset { dim::(Int,Int) , set::[Int] } deriving Show

baseCode:: Char -> Int
baseCode c =
    case c of
        'A' -> 3
        'C' -> 5
        'G' -> 7
        'T' -> 11
        _   -> 0

createDataset:: [[Char]] -> Dataset
createDataset td = Dataset ((length td) ,(length (head td))) (Prelude.map baseCode (L.concat td))

dataMatrix d = fromListUnboxed ((Z :. i) :. j) lst
    where
        i = fst $ dim d :: Int
        j = snd $ dim d :: Int
        lst = set d :: [Int]

markA::Int -> Int
markA e = if e == 3 then 1 else 0

markC::Int -> Int
markC e = if e == 5 then 1 else 0

markG::Int -> Int
markG e = if e == 7 then 1 else 0

markT::Int -> Int
markT e = if e == 11 then 1 else 0

individArrays d =
    [Data.Array.Repa.map markA (dataMatrix d),
     Data.Array.Repa.map markC (dataMatrix d),
     Data.Array.Repa.map markG (dataMatrix d),
     Data.Array.Repa.map markT (dataMatrix d)]

makeProfile d = Prelude.map (foldS (+) 0 ) $ Prelude.map transpose (individArrays d)

listProfile d = Prelude.map toList (makeProfile d)

showProfile p = unlines $ L.map inner p 
    where inner l = L.intersperse ' ' $ foldl (L.++) "" $ L.map show l


data Counter = 
    Counter {   cA::Int
              , cC::Int
              , cG::Int
              , cT::Int
            } deriving (Eq,Show)

data Updater = A | C | G | T | None deriving (Eq,Show)

data Compare = Compare { val::Int, rep::String }

instance Eq Compare where
    (==) c1 c2 = val c1 == val c2
    (/=) c1 c2 = val c1 /= val c2

instance Ord Compare where
    compare c1 c2 = compare (val c1) (val c2)

emptyCounter::Counter
emptyCounter = Counter 0 0 0 0

updateCounter::Updater -> Counter -> Counter
updateCounter u old =
    case u of
        A -> old { cA = (cA old) + 1 }
        C -> old { cC = (cC old) + 1 }
        G -> old { cG = (cG old) + 1 }
        T -> old { cT = (cT old) + 1 }
        None -> old
    

conS:: String -> [Int] -> [Int] -> [Int] -> [Int] -> String
conS s [] [] [] [] = s
conS s (x:xs) (w:ws) (y:ys) (z:zs) = conS ((Prelude.++) s compute) xs ws ys zs
    where
        compute = grater x w y z
        grater a c g t
            | a > c && a > g && a > t = "A"
            | c > a && c > g && c > t = "C"
            | g > a && g > c && g > t = "G"
            | t > a && t > c && t > g = "T"
            | otherwise = rep $ maximum $ reverse [Compare a "A",Compare c "C" ,Compare g "G",Compare t "T" ]

--conSWrapper:: String -> [Int] -> [Int] -> [Int] -> [Int] -> String
--conSWrapper s as cs gs ts = conS s as cs gs ts emptyCounter

consensus a = conS "" (one a) (two a) (three a) (four a)

one::[[Int]] -> [Int]
one = head
two::[[Int]] -> [Int]
two = head . drop 1
three::[[Int]] -> [Int]
three = head . drop 2
four::[[Int]] -> [Int]
four = head . drop 3
