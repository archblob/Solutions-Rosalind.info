module CTBL where

import Data.List (sort)
import qualified Data.Map as M
import Prelude hiding (readFile,writeFile,lines)
import Data.Tree
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 hiding (filter,sort,foldl,foldr,zip,replicate,length,null,map,unwords,unlines)
import Data.Attoparsec.Combinator
import Control.Applicative

testData::ByteString
testData = pack "(dog,((elephant,mouse),robot),cat);"


walk::(a -> [NWTree] -> a) -> a -> NWTree -> a
walk f c t = go c $ subForest t
    where
        go  c0 []  = c0 
        go  c0  nL = go (f c0 nL) newNL
            where
                newNL = foldr ((++) . subForest) [] nL

ctbl::NWTree -> [M.Map ByteString Int]
ctbl tr = walk genRow [] tr 
    where
        len = length txl
        txl = justLabeled tr -- taxa list
        genRow c forest = foldl (\acm (Node l f) -> if null f then acm else  updateMap (justLabeled (Node l f)):acm ) c forest
            where
                updateMap = foldl (\m lbl -> M.insert lbl 1 m) cMap
                    where
                        cMap = M.fromList $ zip txl $ replicate len 0

justLabeled::NWTree -> [ByteString]
justLabeled = sort . filter ((/=) (pack "*")) . flatten

prettyCTBL::[M.Map ByteString Int] -> String
prettyCTBL = unlines . map ( foldr ((++) . show) []  . M.elems )

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
