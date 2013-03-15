module PMCH where

ag::String -> (Integer,Integer)
ag na = ag' na (0,0)
    where
        ag' [] c = c
        ag' (x:xs) (a,g)
            | x == 'A' = ag' xs (a+1,g)
            | x == 'G' = ag' xs (a,g+1)
            | otherwise = ag' xs (a,g)

perfectMatchings::(Integer,Integer) -> Integer
perfectMatchings (a,g) = fact a *  fact g

fact::Integer -> Integer
fact n = product [1..n]

testS::String
testS = "AGCUAGUCAUAUAUAUAUAGUCCCCUUUAAAGGGAGCUAGUCAUAUAUAUAUAGUCCCCUUUAAAGGGACUGGCGGCC"
