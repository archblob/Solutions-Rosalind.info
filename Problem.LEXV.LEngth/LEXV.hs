module LEXV where

import Data.List
import qualified Data.ByteString.Char8 as B

type Alphabet = B.ByteString

realData::String
realData = "IAWFKNTQXS"

writeLexv:: String -> Int -> IO ()
writeLexv s n = do
    let result = unlines $ lexv s n
    writeFile "result" result

lexv s n = sortBy (order' (B.pack s)) $ prod s n

prod:: String -> Int -> [String]
prod s n = product' s n []
    where
        product' s 0 c = c
        product' s n c = product' s (n-1) (c ++ (sequence ( replicate n s)))

sampleData::String
sampleData = "DNA"

order'::Alphabet -> String -> String -> Ordering
order' a [] [] = EQ 
order' a []  _ = LT
order' a _  [] = GT
order' a (x:xs) (y:ys)
    | B.elemIndex x a < B.elemIndex y a = LT
    | B.elemIndex x a > B.elemIndex y a = GT
    | otherwise = order' a xs ys

sampleOutput::[String]
sampleOutput = ["D","DD","DDD","DDN","DDA","DN","DND","DNN","DNA","DA","DAD","DAN","DAA","N","ND","NDD","NDN","NDA","NN","NND","NNN","NNA","NA","NAD","NAN","NAA","A","AD","ADD","ADN","ADA","AN","AND","ANN","ANA","AA","AAD","AAN","AAA"]
