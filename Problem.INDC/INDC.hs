module INDC where

import Text.Printf
import Numeric


fact::Integer -> Integer
fact 0 = 1
fact n = foldl (*) 1 [1..n]

comb::Integer -> Integer -> Integer
comb n k = foldl (*) 1 [n,n-1..(1+(n-k))] `div` fact k

log10 = logBase 10

probSum::Integer -> Double -> Integer -> Double
probSum n ip k = log10 $ foldl (\c x -> indvP x + c) 0 ([k..(2*n)]::[Integer])
    where
        indvP i = (fromInteger (comb (2*n) i)) * (ip^2)

indc::Integer -> [Double]
indc n  = map (probSum n oneProb) [1..(2*n)]
    where
        oneProb = 1.0 / (2^n) :: Double

printIndc:: [Double] -> String
printIndc = unwords . map (printf "%.7f")

writeResult::String -> Integer -> IO ()
writeResult path = writeFile path . printIndc . indc
