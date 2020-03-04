module Main where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

sampleDataset :: Set.Set (Integer, Integer)
sampleDataset = Set.fromList [(6, 6), (4, 6), (6, 5), (4, 3), (3, 5), (2, 1), (1, 4)]

bfs :: Set.Set (Integer, Integer) -> Map.Map Integer Integer
bfs edges = bfs' (Set.singleton 1) 0 edges Map.empty
  where
    bfs' toBeVisited distance es distances
      | Set.null es = distances
      | Set.isProperSubsetOf es'' es = bfs' toBeVisited'' (distance + 1) es'' adjustedDistances
      | otherwise = adjustedDistances
        where (toBeVisited'', visited', es'', distances') = Set.foldr (searchLevel toBeVisited distance) (Set.empty, Set.empty, Set.empty, distances) es
              adjustedDistances = Set.foldr (\(e, d) ds -> Map.insertWith min e d ds) distances' (Set.map (\a -> (a, distance)) (Set.difference toBeVisited visited'))

searchLevel :: Set.Set Integer
            -> Integer
            -> (Integer, Integer)
            -> (Set.Set Integer, Set.Set Integer, Set.Set (Integer, Integer), Map.Map Integer Integer)
            -> (Set.Set Integer, Set.Set Integer, Set.Set (Integer, Integer), Map.Map Integer Integer)
searchLevel toBeVisited distance (from, to) (toBeVisited', visited, es', found)
  | Set.member from toBeVisited = (if to /= from then Set.insert to toBeVisited' else toBeVisited', Set.insert from visited, es', Map.insertWith min from distance found)
  | otherwise = (toBeVisited', visited, Set.insert (from, to) es', found)

ppResult :: [(Integer, Integer)] -> Integer -> [Integer]
ppResult [] _ = []
ppResult [(i, d)] mx = d : replicate (fromInteger (mx - i - 1)) (-1)
ppResult ((qi, qd):(ri, rd):rs) mx
  | qi + 1 /= ri = (qd : replicate (fromInteger (ri - qi - 1)) (-1)) ++ ppResult ((ri, rd):rs) mx
  | otherwise = qd : ppResult ((ri, rd):rs) mx

main :: IO ()
main = do
    [input, output] <- getArgs
    (edges, dataset) <- loadDataset input
    let result  = ppResult (Map.assocs $ bfs dataset) edges
    writeFile output (unwords $ map show result)
    putStrLn "Done!"

loadDataset :: String -> IO (Integer, Set.Set (Integer, Integer))
loadDataset input = do
    inputData <- readFile input
    let (header : datas) = lines inputData
    let [vn, en] = map read $ words header
    return $ (vn, Set.fromList $ map ((\(f:t:_) -> ((read f)::Integer, (read t)::Integer)) . words) datas)
  