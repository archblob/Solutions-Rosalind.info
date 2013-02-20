module Main(main) where

import TRAN
import System.Environment(getArgs)
import qualified Data.ByteString.Lazy.Char8 as B
import Bio.Sequence.Fasta
import Bio.Core.Sequence

dnaString:: Sequence -> B.ByteString
dnaString (Seq _ sd _) = unSD sd

main::IO ()
main = do
    (datasetF:resultF:_) <- getArgs
    sequence <- readFasta datasetF
    let sq1 = dnaString $ head sequence
    let sq2 = dnaString $ head (tail sequence)
    let result = ratioTsTv sq1 sq2
    putStrLn $ show $ (realToFrac (fst result))/(realToFrac (snd result))

