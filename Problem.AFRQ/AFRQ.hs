module AFRQ where

import Text.Printf

pa::Double -> Double
pa n = n + r
    where
        q = sqrt n
        p = 1 - q
        r = (1 - n) - (p^2)

calcProbs::[Double] -> [Double]
calcProbs = map pa

showProbs::[Double] -> String
showProbs = unwords . map (printf "%.3f")
