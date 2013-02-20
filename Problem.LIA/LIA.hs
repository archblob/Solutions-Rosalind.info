module LIA where


factorial::Integer -> Integer
factorial n = fact n 1

fact::Integer -> Integer -> Integer
fact 0 r = r
fact n r = fact (n-1) (n*r)

comb::Integer -> Integer -> Integer
comb n k = factorial n `div` ( factorial k * ( factorial (n-k)))


comb'::Integer -> Integer -> Integer
comb' n k = product [((n-k)+1)..n] `div` factorial k

probALN::Integer -> Integer -> Float
probALN k n = foldl (\c i -> c + instanceProb nG i) 0.0 [n..nG]
    where
        nG = 2^k

instanceProb::Integer -> Integer -> Float
instanceProb nG n = fromInteger (comb' nG n) * (0.25^n) * (0.75^(nG-n))

--- RSTR - Matching RandomMotifs

probFromGC::Double -> String -> Double
probFromGC gc s = foldl which 1.0 s
    where
        gORc = gc / 2
        aORt = (1 - gc) / 2
        which c e
            | e == 'A' || e == 'T' = c * aORt
            | otherwise = c * gORc

randomMotifs::Integer -> Double -> String -> Double
randomMotifs n gc s = foldl withPlus 0.0 [1..50]
    where
        p = probFromGC gc s
        notP = 1 - p
        withPlus c k = c + ((fromInteger (comb' n k)) * (p^k) * (notP^(n-k)))

