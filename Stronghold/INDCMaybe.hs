module INDC where

factorial::Integer -> Integer
factorial = fact 1

fact::Integer -> Integer -> Integer
fact c 0 = c
fact c n = fact (n*c) (n-1)

comb::Integer -> Integer -> Integer
comb n k = product [((n-k)+1)..n] `div` (factorial k)

commonLog::Double -> Double
commonLog = logBase 10




