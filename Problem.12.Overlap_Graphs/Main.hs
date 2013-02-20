module Main(main) where

import System.Environment(getArgs)
import OverlapGraphs
import RosalindFASTA
import Text.ParserCombinators.Parsec
import Text.Parsec.String

main::IO ()
main = do
    [dataF,resultF] <- getArgs
    Right pdata <- parseFromFile parseRFASTAFile dataF
    let result = overlapGraphs pdata
    writeFile resultF (fPretty result)


fPretty::[(RosalindID,RosalindID)] -> String
fPretty [] = ""
fPretty (x:xs) = ((show (fst x)) ++ " " ++ (show (snd x)) ++ "\n") ++ (fPretty xs)


