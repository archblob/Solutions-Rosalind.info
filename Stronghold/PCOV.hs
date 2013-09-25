module PCOV where

import Control.Applicative hiding (empty)
import Prelude hiding (foldr,null,lookup)
import Data.Map hiding (foldl)

deBruijn::[String] -> Map String String
deBruijn = foldl (\c e -> insert (init e) (tail e) c) empty

pcov::[String] -> Maybe String
pcov s = reverse <$> follow "" (init (head s)) dB
    where
        dB = deBruijn s
        follow s0 n0 m0
            | null m0 = Just s0
            | Just v <- lookup n0 m0 = follow ((last n0):s0) v (delete n0 m0)
            | otherwise = Nothing
        
