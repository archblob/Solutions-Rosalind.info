module Main(main) where

import System.Environment (getArgs)
import ConsensusProfile

main::IO ()
main = do
    [datasetF,resultF] <- getArgs
    dataset <- readFile datasetF
    let daTa = lines dataset
    let profile = listProfile $ createDataset daTa
    let cons = consensus profile
    writeFile resultF $ cons ++ "\n" ++ (showProfile profile)
    putStrLn "Done!"
