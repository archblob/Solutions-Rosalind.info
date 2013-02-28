module Main(main) where

import System.Environment(getArgs)
import TRIE

main:: IO ()
main = do
    (dF:rF:_) <- getArgs
    rawData <- readFile dF
    let wordList = lines $  rawData
    let wordTrie = foldl insert' empty wordList
    let result   = adjToString $ adjacencyList wordTrie
    writeFile rF result

