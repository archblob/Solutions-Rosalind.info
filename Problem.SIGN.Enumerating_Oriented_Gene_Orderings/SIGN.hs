module SIGN(
      sign
    , listsToString
    ) where

import Data.List

sign::Int -> [[Int]]
sign n = go [] (subsequences set)
    where
        set     = [1..n]::[Int]
        go c [] = c
        go c (x:xs) = go newC xs
            where
                newC = c ++ permutations ((set \\ x) ++ (map (* (-1)) x))

listsToString::[[Int]] -> String
listsToString = foldl go ""
    where
        go s e = s ++ (init (foldl (\c v -> c ++ (show v) ++ " ") "" e )) ++ "\n"
