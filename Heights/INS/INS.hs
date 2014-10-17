module INS where

fullSwap (_, n, True)    = n
fullSwap (fnl, n, False) = fullSwap (halfSwap fnl)
  where
    halfSwap :: [Int] -> ([Int], Int, Bool)
    halfSwap (x:y:xs)
      | x > y     = (y : sl, ns + 1, False)
      | otherwise = (x : nl, nn, f)
        where
          (sl, ns, _) = halfSwap (x:xs)
          (nl, nn, f) = halfSwap (y:xs)
    halfSwap ls = (ls, n, True)
