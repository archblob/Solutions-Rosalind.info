module PAR where


twowayPartition :: [Int] -> [Int]
twowayPartition (pivot:xs) = part xs (\e -> pivot:e)
  where
    part [] cnt = cnt []
    part (x:xs) cnt
      | x <= pivot = x : part xs cnt
      | otherwise  = part xs (\e -> cnt (x:e))


