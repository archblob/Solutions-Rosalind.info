module QRT where

import Data.List
import qualified Data.Vector as V
import Data.Array.IArray

data Quartet = Quartet [String] [String]

instance Eq Quartet where
    (==) (Quartet l0 r0) (Quartet l1 r1) = (eq l0 l1 && eq r0 r1) || (eq l0 r1 && eq r0 l1)
        where
            eq a b = all ((flip elem) b) a

instance Show Quartet where
    show (Quartet l r) = "{" ++ toS l ++ "} {" ++ toS r ++ "}"
        where
            toS = concat . intersperse ", "


dup::Eq a => [a] -> [a]
dup [] = []
dup (x:xs) = x : dup (filter (\y -> x/=y) xs)

qrtToString::[Quartet] -> String
qrtToString = unlines . map show

qrt::[String] -> [Quartet]
qrt (x:xs) = dup $  foldr ((++) . formQ) [] xs
    where
        taxaL   = words x ::[String]
        tLength = length taxaL ::Int
        taxaA   = array (1,tLength) $ zip [1..tLength] taxaL :: Array Int String
        formQ p = quart $ part $ V.fromList p
        quart (a,b) = cartez (twoComb a) (twoComb b)
        part  p = V.ifoldl decide ([],[]) p
            where
                decide (a,b) i '1' = (insert (taxaA ! (i+1)) a ,b)
                decide (a,b) i '0' = (a,insert (taxaA ! (i+1)) b)  
                decide (a,b) i 'x' = (a,b)

cartez a b = [Quartet x y | x <- a, y <- b]

twoComb = combinations 2

combinations 0 _ = [[]]
combinations _ [] = []
combinations n xs@(y:ys)
  | n < 0     = []
  | otherwise = case drop (n-1) xs of
                  [ ] -> []
                  [_] -> [xs]
                  _   -> [y:c | c <- combinations (n-1) ys]
                            ++ combinations n ys
