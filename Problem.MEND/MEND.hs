module MEND where

import Prelude hiding (foldl,length,readFile,writeFile,lines)
import Data.Tree
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8
import Data.Attoparsec.Combinator
import Control.Applicative


compute:: String -> IO (Float,Float,Float)
compute s = do
    f <- readFile s
    let dataset = Prelude.head $ lines f 
    let pData = either (error "could not parse") (id) (parseNWK dataset)
    return $ calc pData

calc:: Tree ByteString -> (Float,Float,Float)
calc (Node s []) = singleProbs s
calc (Node _ [Node a aL , Node b bL ]) = nodeProbs (calc (Node a aL)) (calc (Node b bL))

singleProbs::ByteString -> (Float,Float,Float)
singleProbs s
    | s == pack "AA" = (1.0,0.0,0.0)
    | s == pack "Aa" = (0.0,1.0,0.0)
    | otherwise = (0.0,0.0,1.0)

nodeProbs::(Float,Float,Float) -> (Float,Float,Float) -> (Float,Float,Float)
nodeProbs (a,b,c) (a0,b0,c0) =
    ( a * a0 + a * 0.5 * b0 + b * 0.5 * a0 + b * 0.25 * b0 ,
      a * 0.5 * b0 + a * c0 + b * 0.5 * a0 + b * 0.25 * b0 + b * 0.25 * b0 + b * 0.5 * c0 + c * a0 + c * 0.5 * b0 ,
      b * 0.5 * b0 * 0.5 + b * 0.5 * c0 + c * 0.5 * b0 + c * c0)

-- NEWICK Tree parsing without weigths -- 
type NWTree = Tree ByteString

stdLabel::ByteString
stdLabel = pack "*"

parseNWK:: ByteString -> Either String NWTree
parseNWK = parseOnly nwTree

nwTree = do
    forest <- descendant_list
    rl <- option (stdLabel) label
    char ';'
    return $ Node rl forest

descendant_list = do
    char '('
    st <- subtree `sepBy` (char ',')
    char ')'
    return st

subtree = internal <|> leaf

label = do
    l <- takeWhile1 $ inClass "_0-9a-zA-Z"
    return l

-- Working -- 
internal = do
    forest <- descendant_list 
    nl <- option stdLabel label
    return $ Node nl forest

-- Working --
leaf = do
    nl <- option stdLabel label
    return $ Node nl []
