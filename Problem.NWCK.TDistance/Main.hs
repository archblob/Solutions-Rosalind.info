module Main(main) where

import NWCK
import Text.Printf
import System.Environment(getArgs)
import qualified Data.Text as T

main:: IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    rawDataset <- readFile datasetF
    let dataset   = filter (\x -> x /= "" || x /= "\r") $ lines rawDataset
    let triadList = makeTriad dataset
    let result = unwords $ map ( printf "%0.f" . (maybe (-1.0) id)) $ evalTriad triadList
    writeFile resultF result

makeTriad::[String] -> [(Either String NWTree,T.Text,T.Text)]
makeTriad [] = []
makeTriad (t:ab:xs) = (parseNWK (T.pack t),T.pack a, T.pack b): makeTriad xs 
    where
        (a:b:_) = words ab

evalTriad:: [(Either String NWTree,T.Text,T.Text)] -> [Maybe Double]
evalTriad = map evl

evl::(Either String NWTree,T.Text,T.Text) -> Maybe Double
evl ((Right t),a,b) = fmap pathCost $ pathFromAtoB t a b
evl ((Left s),_,_)  = error s
