module LCSQ where

import Data.Array
-- import Data.Array.Repa
import qualified Data.ByteString.Char8 as B

-- Sample Data --

--s :: B.ByteString
s = "AACCTTGG"
--p :: B.ByteString
p = "ACACTGTGA"

-- END Sample Data --


longest::B.ByteString -> B.ByteString -> B.ByteString
longest s p = if B.length s > B.length p then s else p

longest':: String -> String -> String
longest' s p = if length s > length p then s else p

lcs::B.ByteString -> B.ByteString -> B.ByteString
lcs s p
    | B.null s || B.null p = B.empty
    | B.head s == B.head p = B.cons (B.head s) $! lcs (B.tail s) (B.tail p)
    | otherwise = seq left   $ longest left $! right
        where
            left  = lcs s  (B.tail p)
            right = lcs (B.tail s) p

{-
lcs' s p = nullArray
    where
        nullArray = fromListUnboxed ((Z :. sL) :. sP) $! replicate (sL*sP) 0
        sL  = B.length s
        sP  = B.length p
-}

lcs'' xs ys = a!(0,0) where
    n = length xs
    m = length ys
    a = array ((0,0),(n,m)) $ l1 ++ l2 ++ l3
    l1 = [((i,m),[]) | i <- [0..n]]
    l2 = [((n,j),[]) | j <- [0..m]]
    l3 = [((i,j), f x y i j) | (x,i) <- zip xs [0..], (y,j) <- zip ys [0..]]
    f x y i j 
        | x == y    = x : a!(i+1,j+1)
        | otherwise = longest' (a!(i,j+1)) (a!(i+1,j))
