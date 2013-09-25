module SUBS (
    findMotif
    ) where


findMotif::Eq a => [a] -> [a] -> [Int]
findMotif s t = go s 1 []
    where
        go [] _ p = p
        go l@(x:xs) c p
            | sample == t = go xs (c+1) (p ++ [c])
            | otherwise = go xs (c+1) p
                where
                    sample = take subl l
        subl = length t
