module RosalindFASTA (
      parseRFASTAFile
    , RosalindID
    , DNASequence
    , RFASTA
    , idR
    , rid
    , dnaS
    , dna
    ) where

import Data.List
import Text.ParserCombinators.Parsec
import Text.Parsec.String
import Text.Parsec.Char

type DNAString = String

data RosalindID = RID  { idR::String } deriving Eq

data DNASequence = DNA { dna:: String } deriving Eq

data RFASTA = RFASTA { rid::RosalindID , dnaS::DNASequence } deriving Eq


instance Show RosalindID where
    show r = (idR r)

instance Show DNASequence where
    show s = ":: " ++ (dna s) ++ "\n"

instance Show RFASTA where
    show rf = "-------------\n" ++ (show (rid rf)) ++ (show (dnaS rf))


parseRosalindID:: GenParser Char st RosalindID
parseRosalindID = do
    string ">"
    ident <- manyTill anyChar newline
    return $ RID ident

parsePartialDNASequence:: GenParser Char st String
parsePartialDNASequence = do
    str <- many1 (oneOf "ACTG")
    skipMany1 newline
    return str

parseDNASequence:: GenParser Char st DNASequence
parseDNASequence = do
    ls <- many1 parsePartialDNASequence
    return $ DNA $ concat ls

parseRFASTA:: GenParser Char st RFASTA
parseRFASTA = do
    rosid <- parseRosalindID
    dnaseq <- parseDNASequence
    return $ RFASTA rosid dnaseq

parseRFASTAFile:: GenParser Char st [RFASTA]
parseRFASTAFile = do
    list <- manyTill parseRFASTA eof
    return list
