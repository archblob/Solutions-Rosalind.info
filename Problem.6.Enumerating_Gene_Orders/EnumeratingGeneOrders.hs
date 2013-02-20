module EnumeratingGeneOrders (
    writePerm
    ) where

import Data.List

writePerm = writeFile "rezultat" $ unlines $ permutations "12345"
