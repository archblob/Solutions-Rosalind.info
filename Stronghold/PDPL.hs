module PDPL (
    maybePdpl
    ) where

import Control.Applicative ((<|>), (<$>))
import Data.IntMultiSet (IntMultiSet)
import Data.IntSet (IntSet)
import qualified Data.IntMultiSet as MS
import qualified Data.IntSet as IS

-- Creating a Restriction Map

-- TODO : check should terminate on the first Nothing
-- maybe write foldM for IntSet ?
-- Try also to check and delete separately
maybePdpl :: IntMultiSet -> Maybe IntSet
maybePdpl d0 = go maxd0 (IS.singleton 0) d0
    where
        maxd0 = MS.findMax d0
        go pPrev p1 d1
            | MS.null d1 = Just p1
            | otherwise  = check >>= tryNext
                where
                    tryNext d2 = let pr = MS.findMax d2
                                in go (maxd0 - pr) p2 d2 <|> go pr p2 d2
                    p2    = IS.insert pPrev p1
                    check = IS.foldl validDistance (Just d1) p1
                    validDistance acm cP = acm >>= isMember
                        where
                            isMember d
                                | MS.member dist d = Just $ MS.delete dist d
                                | otherwise        = Nothing
                            dist = abs $ pPrev - cP
