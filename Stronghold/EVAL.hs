module EVAL where

import Numeric

factorial:: Integer -> Integer
factorial = fact 1

fact::Integer -> Integer -> Integer
fact f 0 = f
fact f n = f' `seq` (fact f' (n-1))
    where
        f' = n*f

comb::Integer -> Integer -> Integer
comb n k = product [((n-k)+1)..n] `div` (factorial k)

probWithGC::String -> Double -> Double
probWithGC s gc = foldl withProb 1.0 s
    where
        gORc = gc / 2
        aORt = (1 - gc) / 2
        withProb c b
            | b == 'A' || b == 'T' = c * aORt
            | otherwise = c * gORc

eventSum::Integer -> Integer -> Double -> Double
eventSum l pN p = foldl individSum 0.0 [1..30]
    where
        notP = 1 - p
        n = (pN - l) + 1
        individSum c e = c + ((fromInteger e) * (fromInteger (comb n e)) * (p^e) * (notP^(n-e)))

computeExpected::Integer -> String -> [Double] -> [Double]
computeExpected n s lst = map computePoint lst
    where
        l = toInteger $ length s
        computePoint = eventSum l n . probWithGC s

prepareData::String -> IO (Integer,String,[Double])
prepareData path = do
    rawData <- readFile path
    let [l,s,ps] = lines rawData
    let nL = read l ::Integer
    let nPS = map (fst . head .readFloat) $ words ps ::[Double]
    return (nL,s,nPS)

stringify::[Double] -> String
stringify = unwords . map show 
