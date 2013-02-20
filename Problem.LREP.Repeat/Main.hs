module Main(main) where

import qualified Data.Map as M
import System.Environment(getArgs)
import Debug.Trace

main::IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    rawData <- readFile datasetF
    let dataset = construct $ lines rawData
    -- let result = mRepeat dataset
    -- writeFile resultF result
    putStrLn $ show $ highest dataset

data Proc = 
    Proc { 
      str::String
    , mx::Int
    ,nodes::M.Map (Int,Int) Int
    } deriving (Eq,Show)

construct::[String] -> Proc
construct s = Proc st n $ makeMap nds 
    where
        st = head s
        n  = read $ head $ tail s
        nds = map (justInts . drop 2 . words) $ drop 2 s

justInts::[String] -> (Int,Int)
justInts (x:y:_) = (read x , read y)

makeMap::[(Int,Int)] -> M.Map (Int,Int) Int
makeMap = foldl ins M.empty
    where
        ins c s = M.insertWith (+) s 1 c

highest::Proc -> Int
highest p = M.foldl (\c i -> if i > c then i else c) 0 (nodes p)

mRepeat::Proc -> String
mRepeat p = getInterval' (str p) interv
    where
        interv = fst $ M.foldlWithKey f ((0,0),0) $ traceShow flt flt
        flt = M.filter (\x -> x >= mx p) (nodes p)
        f old@((_,l),n') k n
            | snd k > l = (k,n)
            | otherwise = old

getInterval'::String -> (Int,Int) -> String
getInterval' s (x',y') = take y $ drop x s 
    where
        x = x' - 2
        y = y'
