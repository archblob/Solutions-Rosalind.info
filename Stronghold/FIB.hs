module FIB where


-- Fibonacci from definition
fib::Integer -> Integer
fib 1  = 1
fib 2  = 1
fib n  = fib (n-1) + fib (n-2)

-- Remember only the last two values
-- Also with dynamic litter size
fibo::Integer -> Integer -> Integer
fibo n lit = fib0 0 1 n
    where
        fib0 lp1 lp2 c
            | c <= 2 = lp1 + lp2
            | otherwise = fib0 (lit*lp2) (lp1+lp2) (c-1)
