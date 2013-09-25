module Main(main) where

import System.Environment(getArgs)
import qualified Data.Vector.Unboxed as V

main::IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    rawData <- readFile datasetF
    let (n:vl:_) = lines rawData
    let t  = read n :: Double
    let pL = V.fromList $ map read $ words vl :: V.Vector Double
    let result = V.map ((* t)) pL
    writeFile resultF $ unwords $ V.foldr (\x c -> (show x):c) [] result 

fac:: Integer -> Integer
fac 0 = 1
fac n = product [1..n]

comb:: Integer -> Integer -> Integer
comb n k = (product [((n-k)+1)..n]) `div` ((fac k))

probK::Integer -> Float -> Integer -> Float
probK n p k = (fromInteger (comb n k)) * (p^k) * ((1.0 - p)^(n-k))

expectedV::Integer -> Float -> Float
expectedV n p = sum $ map (probK n p) [0..n]
