module Main(main) where

import System.Environment(getArgs)
import Network.HTTP
import Network.Browser
import Control.Applicative
import qualified Data.ByteString.Char8 as B

main::IO ()
main = do
   (datasetF:_) <- getArgs
   rawDataset <- readFile datasetF
   let dataset = lines rawDataset
   mprt dataset

n_glycoMotif:: B.ByteString -> Bool
n_glycoMotif s
    | B.length s /= 4 = False
    | n && p 1 && sT && p 3 = True
    | otherwise = False
        where
            n = B.index s 0 == 'N'
            p i = B.index s i /= 'P'
            sT = B.index s 2 == 'S' || B.index s 2 == 'T'


getDNASequence:: String -> B.ByteString
getDNASequence = B.pack . concat . tail . lines

getSequences:: [String] -> IO [B.ByteString]
getSequences s = do
    let origin x  = "http://www.uniprot.org/uniprot/" ++ x ++ ".fasta"
    let toSequence x =  (rspBody . snd) <$> (browse (request (getRequest (origin x))))
    sq  <- mapM toSequence s
    --sq' <- sequence sq
    let rfasta = map getDNASequence sq
    return $ rfasta

findMotif::B.ByteString -> [Int]
findMotif s = locationList s 0 []
    where
        locationList s i l
            | B.null s = l
            | B.head s  /= 'N' = locationList (B.drop 1 s) (i+1) l
            | n_glycoMotif (B.take 4 s) = locationList (B.drop 1 s) (i+1) (l ++ [i+1])
            | otherwise = locationList (B.drop 1 s) (i+1) l
        

findMotifs::[B.ByteString] -> [[Int]]
findMotifs = map findMotif

mprt d = do
    sq <- getSequences d
    let result = findMotifs sq
    B.writeFile "result"  $ formatTupleList  $ zip (map B.pack d) result

formatTupleList::[(B.ByteString,[Int])] -> B.ByteString
formatTupleList = B.unlines . map spaghetti 
    where
        spaghetti x = if length (snd x) > 0 then B.append (fst x) (B.pack ( "\n" ++ (foldl (\c e -> c ++ (show e) ++ " ") "" (snd x))) ) else B.empty
