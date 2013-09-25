module EDIT where

import qualified Data.ByteString.Char8 as B

levenshtein s1 s2 = last $ foldl transform [0 .. length s1] s2
  where transform ns@(n:ns') c = scanl calc (n+1) $ zip3 s1 ns ns'
            where calc z (c', x, y) = minimum [y+1, z+1, x + fromEnum (c' /= c)]


s::B.ByteString
s = B.pack "MARTYNX"
t::B.ByteString
t = B.pack "FAMASTRYNC"

editDistance::B.ByteString -> B.ByteString -> Int
editDistance s t = sL - (inOrder s t 0)
    where
        sL = B.length s

inOrder::B.ByteString -> B.ByteString -> Int -> Int
inOrder s t c
    | B.null t = c
    | Just i <- B.elemIndex (B.head t) s = inOrder (B.drop (i+1) s) (B.tail t) (c+1)
    | otherwise = inOrder s (B.tail t) c

lvshD::B.ByteString -> B.ByteString -> Int
lvshD s t
    | len_s == 0 = len_t
    | len_t == 0 = len_s
    | otherwise = minimum [lvshD (B.take (len_s-1) s) t + 1 , lvshD s (B.take (len_t-1) t) + 1 , lvshD (B.take (len_s-1) s) (B.take (len_t-1) t) + cost]
        where
            len_s = B.length s
            len_t = B.length t
            cost  = if (B.head s) /= (B.head t) then 1 else 0

