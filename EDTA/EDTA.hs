module EDTA where

import Data.Array
import qualified Data.ByteString.Char8 as B

edta::String -> String -> (Int,(String,String))
edta s t = result
    where
        result = alignmentTable (B.pack s) (B.pack t) (-1) (0) (-1)

alignmentTable :: B.ByteString -> B.ByteString 
               -> Int -> Int -> Int
               -> (Int, (String, String))
alignmentTable p0 q0 gap match nomatch = ( abs $ table ! (lenp,lenq) ,retrieve lenp lenq ("",""))
  where
    table = listArray ((0,0),(lenp, lenq)) [score x y | x <- [0..lenp], y <- [0..lenq]]
    p = B.cons ' ' p0
    q = B.cons ' ' q0
    lenp = B.length p0
    lenq = B.length q0

    score :: Int -> Int -> Int
    score 0 0 = 0
    score 0 j = gap * j
    score i 0 = gap * i
    score i j = maximum [ table ! (i-1, j-1) + same i j , table ! (i-1, j) + gap , table ! (i, j-1) + gap ]

    same i j
      | B.index p i == B.index q j = match
      | otherwise = nomatch

    retrieve :: Int -> Int -> (String,String) -> (String,String)
    retrieve 0 0 c     = c
    retrieve 0 j (s,t) = retrieve 0 (j-1) ('-' : s, B.index q j : t)
    retrieve i 0 (s,t) = retrieve 0 (i-1) (B.index p i : s, '-' : t)
    retrieve i j (s,t)
      | current - gap   == table ! (i, j-1)   = retrieve i (j-1) ('-' : s, B.index q j : t)
      | current - gap   == table ! (i-1, j)   = retrieve (i-1) j (B.index p i : s, '-' : t)
      | current - match == table ! (i-1, j-1) = retrieve (i-1) (j-1) (B.index p i : s, B.index q j : t)
      | current - nomatch == table ! (i-1,j-1) = retrieve (i-1) (j-1) (B.index p i : s, B.index q j : t)
      | otherwise = error "WTF"
        where current = table ! (i, j)
