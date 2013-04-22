module ROOT where


-- cases not covered. n = 0 or negative
root::Integer -> Integer
root n = product [n..(2*n-2)] `div` 2^(n-1) `mod` 1000000
