module CSTR where

import Data.List

cstr::[String] -> [String]
cstr l = reverse $ go [] l
    where
        go sl l
            | null firstSeq = sl
            | otherwise     = go (insertNonTriv snpSplit sl) newL
                where
                    insertNonTriv s l
                        | trivial s = l
                        | otherwise = insert s l
                    firstSeq = head l
                    witness  = head $ firstSeq
                    snpSplit = foldr ((:) . (\e -> if head e == witness then '1' else '0')) "" l
                    newL     = map (drop 1) l

trivial::String -> Bool
trivial s
    | c1 > 1 && c0 > 1 = False
    | otherwise = True
        where
            (c1,c0) = foldl (\(ones,zeros) e -> if e == '1' then (ones + 1, zeros) else (ones, zeros + 1)) (0,0) s
