module Main(main) where

import NWCK
import Text.Printf
import System.Environment(getArgs)
import qualified Data.Text as T
import Data.String.Utils

main:: IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    rawDataset <- readFile datasetF
    let dataset   = stripAndFold $ lines rawDataset
    let triadList = makeTriad dataset
    let result = unwords $ map ( show . (maybe (999) id)) $ evalTriad triadList
    writeFile resultF result


stripAndFold::[String] -> [String]
stripAndFold = foldr dropIfBlank []
    where
        dropIfBlank s c
            | null nS = c
            | otherwise = s : c
                where nS = strip s

makeTriad::[String] -> [(Either String NWTree,T.Text,T.Text)]
makeTriad [] = []
makeTriad (t:ab:xs) = (parseNWK (T.pack t),T.pack a, T.pack b): makeTriad xs 
    where
        [a,b] = words ab

evalTriad:: [(Either String NWTree,T.Text,T.Text)] -> [Maybe Integer]
evalTriad = map evl

evl::(Either String NWTree,T.Text,T.Text) -> Maybe Integer
evl ((Right t),a,b) = fmap pathCost $ pathFromAtoB (uniqueLabels t) a b
evl ((Left s),_,_)  = error s
