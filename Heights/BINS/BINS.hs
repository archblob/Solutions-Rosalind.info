module BINS where

import Data.Array
import System.Environment (getArgs)
import System.IO (readFile)

main :: IO ()
main = do
    (f:o:_)  <- getArgs
    contents <- readFile f
    let (as, ks) = binsFromString contents
    let result   = bins2String $ bins as ks
    writeFile o result


binsFromString :: String -> (Array Int Int, [Int])
binsFromString str = (listArray (1, n) $ map read (words ar), map read (words ks))
    where (ns:_:ar:ks:_) = lines str
          n = read ns :: Int


-- TODO: variant that brakes data structure in  sections so that no manual bound checking is
-- necessary
-- Error prone search using indices
binaryArrayIndexSearch :: Ord a => Array Int a -> a -> Maybe Int
binaryArrayIndexSearch arr k = indexSearch (bounds arr)
  where
    indexSearch (s, e)
      | s > e     = Nothing
      | k == me   = Just middle
      | k > me    = indexSearch (middle + 1, e)
      | otherwise = indexSearch (s, middle - 1)
        where middle = s + (e - s) `div` 2
              me     = arr ! middle

bins :: Array Int Int -> [Int] -> [Maybe Int]
bins arr ks = map (binaryArrayIndexSearch arr) ks

bins2String :: [Maybe Int] -> String
bins2String bns = unwords $ map (maybe "-1" show) bns
