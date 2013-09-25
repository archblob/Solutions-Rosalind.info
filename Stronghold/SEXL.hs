module SEXL where

import Text.Printf

showProbs::[Double] -> String
showProbs = unwords . map (printf "%.4f")

pXk::Double -> Double
pXk p = 2 * p * q
    where
        q = 1 - p

calcProbs::[Double] -> [Double]
calcProbs = map pXk
