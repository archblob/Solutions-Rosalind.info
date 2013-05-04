module ITWV where

itwvTable::String -> [String] -> [[String]]
itwvTable m sL = map (\e -> map (bools . (itwv m e)) sL) sL

tableS::[[String]] -> String
tableS = unlines . map unwords

bools::Bool -> String
bools False = "0"
bools True  = "1"

itwv::String -> String -> String -> Bool
itwv s p1@(x:xs) p2@(y:ys) = go s
    where
        p1len = length p1
        p2len = length p2
        go sequence
            | slen < p1len + p2len = False
            | x == m && interwoven section xs p2 = True
            | y == m && interwoven section p1 ys = True
            | otherwise = go (tail sequence)
                where
                    slen = length sequence
                    (m:ms) = sequence
                    section = take (p1len + p2len - 1) ms

interwoven:: String -> String -> String -> Bool
interwoven [] _ _  = True
interwoven _  _ [] = True
interwoven _  [] _ = True
interwoven (m:ms) p1@(x:xs) p2@(y:ys)
    | x == m && subseq p2 ms = interwoven ms xs p2
    | y == m && subseq p1 ms = interwoven ms p1 ys
    | otherwise = False

subseq::String -> String -> Bool
subseq [] _ = True
subseq _ [] = False
subseq p@(x:xs) (y:ys)
    | x == y = subseq xs ys
    | otherwise = subseq p ys
