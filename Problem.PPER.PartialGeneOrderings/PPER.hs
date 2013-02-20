module PPER where

factorial::(Eq a, Num a) => a -> a
factorial n = go (abs n) 1
    where
        go 0 c = c
        go n c = go (n-1) $! c*n

--partialPermutation::(Eq a,Num a) => a -> a -> a
partialPermutation n k = ((factorial n) `div` factorial (n-k)) `mod` 1000000
