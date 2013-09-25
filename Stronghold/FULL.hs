module FULL where

import Data.List hiding (lookup)
import Prelude hiding (lookup)
import Data.AEq
import Control.Applicative
import Data.Tuple
import qualified Data.Vector as V

monoisotopicMassTable::[(Char,Double)]
monoisotopicMassTable = 
        [('A',71.03711),('C' ,103.00919),('D',115.02694)
        ,('E',129.04259),('F',147.06841),('G',57.02146)
        ,('H',137.05891),('I',113.08406),('K',128.09496)
        ,('L',113.08406),('M',131.04049),('N',114.04293)
        ,('P',97.05276),('Q',128.05858),('R',156.10111)
        ,('S',87.03203),('T',101.04768),('V',99.06841)
        ,('W',186.07931),('Y',163.06333) ]

aproxTable::V.Vector (Double,Char)
aproxTable = V.fromList $ sort $ map swap monoisotopicMassTable

lookup:: Double -> V.Vector (Double,Char) -> Maybe Char
lookup val = (snd <$>) . V.find (\(e,ch) -> val ~== e)

full:: Double -> [Double] -> Maybe String
full p0 mL0 = reverse <$> go "" p0 mL0
    where
        go pept p [x] = Just pept
        go pept p (x:y:xs)
            | Just v <- find = go (v:pept) p newL
            | otherwise = go pept p (x:xs) 
                where
                    newL = filter (cond x y) (y:xs)
                    find = lookup (y - x) aproxTable
                    cond x y e
                        | e ~== (p-x) || e ~== (p-y) = False
                        | otherwise = True
            
