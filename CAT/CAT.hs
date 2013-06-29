module CAT where

import qualified Data.ByteString.Char8 as B
import Data.MemoCombinators (memo2,integral)
import Data.Bits (shiftR)

bind :: Char -> Char -> Integer
bind 'A' 'U' = 1
bind 'U' 'A' = 1
bind 'C' 'G' = 1
bind 'G' 'C' = 1
bind  _   _  = 0

cat::B.ByteString -> Integer
cat s = catalan 1 (toInteger (B.length s)) `mod` 1000000
  where
    catalan = memo2 integral integral catalan0
    index i = B.index s (fromInteger i - 1)
    cmp i j = bind (index i) (index j)

    catalan0 :: Integer -> Integer -> Integer
    catalan0 i j
      | n <= 0 = 1
      | n == 1 = cmp i j
      | otherwise = sum [prod i j j' | k <- [1..lim], let j' = (i-1) + 2*k]
        where
          n   = (j-i) + 1
          lim = n `shiftR` 1
          prod i j j' = cmp i j' * catalan (i + 1) (j'-1) * catalan (j'+1) j
