module TWOSUM where

import Data.List
import Control.Monad

twosum :: [Int] -> Maybe (Int, Int)
twosum lst = do
  n <- existsOpus $ sortBy (\a b -> compare (abs a) (abs b)) lst
  let getIndex (Right a, s) e
        | n == e = (Left a, fmap (+1) s)
      getIndex (a, Right b) e
        | n == (-e) = (fmap (+1) a, Left b)
      getIndex (a, b) _ = (fmap (+1) a, fmap (+1) b)
  case foldl getIndex (Right 1, Right 1) lst of
    (Left a, Left b) -> Just (a, b)
    _ -> Nothing

existsOpus :: [Int] -> Maybe Int
existsOpus []  = Nothing
existsOpus [x] = Nothing
existsOpus (x:y:xs)
  | x + y == 0 = Just x
  | otherwise  = existsOpus (y:xs)
