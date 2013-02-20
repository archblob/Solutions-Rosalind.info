module WFMD where

factorial::Integer -> Integer
factorial n = facs !! n' where n' = fromIntegral n

facs::[Integer]
facs = scanl (*) 1 [1..]

(*&*)::Integer -> Integer -> Integer
(*&*) n k = foldl (*) 1 [(1+(n-k))..n] `div` factorial k

-- Implementations

wfProb::Integer -> Double -> Double -> Integer -> Double
wfProb t p q k = (fromInteger (t *&* k)) * (p^(t - k)) * (q^k)
    

wf::Integer -> Integer -> Integer -> Integer -> Double
wf n m k 0 = 1
wf n m k g = foldr ((+) . (\x -> (wf n (t-x) k (g-1)) * (wfProb t p q x))) 0 [k..t]
    where
        t = 2*n
        p = fromInteger m / (fromInteger t) :: Double
        q = 1 - p

-- Problem :: FOUN - Founder effect and Genetic drift :: solution:

feProb::Integer -> Double -> Double -> Integer -> Double
feProb t p q k = (fromInteger (t *&* k)) * (p^k) * (q^(t-k))

fe::Integer -> Integer -> Integer -> Double
fe n r g
    | g == 0    = 1
    | g == 1    = p^t
    | otherwise = foldr((+) . (\x -> (fe n (t-x) (g-1)) * (feProb t p q x))) 0 [0..t]
    where
        t = 2*n
        q = fromInteger r / (fromInteger t) :: Double
        p = 1 - q

fe'::Integer -> Integer -> Integer -> Double
fe' n r g = logBase 10 $ fe n r g

feArray::Integer -> Integer -> [Integer] -> [[Double]] -> [[Double]]
feArray n 0 ks c = c
feArray n g ks c = feArray n (g-1) ks ((map (\r -> fe' n r g) ks):c)

feA::Integer -> Integer -> [Integer] -> [[Double]]
feA n g ks = feArray n g ks []

showFeArray::[[Double]] -> String
showFeArray = unlines . map (unwords . (map show))

readFe::String -> ((Integer,Integer),[Integer])
readFe s = ((n,g),ks)
    where
        (ng0:ks0:_) = lines s
        ng = words ng0
        n = read $ head ng :: Integer
        g = read $ head $ tail ng :: Integer 
        ks = map read $ words ks0 :: [Integer]
