module IntroductionToProbability (
    sbProbabilityFromGC
    , probFromGCFreqList
    , dataset
    ) where

sbProbabilityFromGC::Float -> Float
sbProbabilityFromGC gc = gP + cP + aP + tP
    where
        gORc = gc / 2 :: Float
        aORt = (1 - gc) / 2 :: Float
        gP   = gORc * gORc
        cP   = gP
        aP   = aORt * aORt
        tP   = aP

dataset::[Float]
dataset = [0.000,0.070,0.142,0.221,0.267,0.306,0.395,0.421,0.471,0.575,0.618,0.653,0.744,0.804,0.874,0.923,1.000]

probFromGCFreqList:: [Float] -> [Float]
probFromGCFreqList = map sbProbabilityFromGC
