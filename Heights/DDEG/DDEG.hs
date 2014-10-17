module DDEG where

import Data.Array
import Data.List

ddeg :: Int -> [(Int, Int)] -> [Int]
ddeg nodes edgeList = map sumOfNeighboursDegree $ elems edgeArray
    where
      edgeArray = accumArray (flip (:)) [] (1, nodes) twoWayEdgeView
        where twoWayEdgeView = foldr (\e@(f,t) acm -> e:(t,f):acm) [] edgeList
      sumOfNeighboursDegree = foldl' addNeighbourDegree 0
        where addNeighbourDegree s = ((+) s) . length . (!) edgeArray 
