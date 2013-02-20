module Main(main) where

import Data.List
import System.Environment(getArgs)
import qualified Data.Set as S

main::IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    rawData <- readFile datasetF
    let [sSize,sS1,sS2] = lines rawData
    let size = read sSize :: Int
    let masterS = S.fromList [1..size]
    let s1   = S.fromList (read sS1::[Int])
    let s2   = S.fromList (read sS2::[Int])
    let aUb  = S.union s1 s2
    let aIb  = S.intersection s1 s2
    let a_b  = S.difference s1 s2
    let b_a  = S.difference s2 s1
    let aC   = S.difference masterS s1
    let bC   = S.difference masterS s2
    let result = map S.toList [aUb,aIb,a_b,b_a,aC,bC]
    writeFile resultF $ unlines (map (\x -> "{" ++ (concat (intersperse ", " (map show x))) ++ "}") result)
    putStrLn "Done!"
    

