module ASPLC where

--factorial:: (Eq a , Num a) => a -> a
factorial n = go n 1
    where
        go 0 f = f
        go v f = go (v-1) $! v*f

--combinations::(Eq a, Num a) => a -> a -> a
combinations n k = (factorial n) `div` ((factorial k) * factorial (n-k))

--combSum:: (Eq a , Num a) => a -> a -> a
combSum n m = (foldl (\c xm -> (combinations n xm) + c) 0 [m..n]) `mod` 1000000
