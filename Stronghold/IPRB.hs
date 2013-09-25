module IPRB where

probDominant:: Double -> Double -> Double -> Double
probDominant k m n = eHH_HH + eHH_Hh + (3/4) * eHh_Hh + (1/2) * eHh_hh + eHH_hh
    where
        p = k + m + n
        eHH_HH  = (k/p)*((k-1)/(p-1)) :: Double
        eHH_Hh  = ((k/p)*(m/(p-1))) + ((m/p)*(k/(p-1))):: Double
        eHH_hh  = (k/p)*(n/(p-1)) + (n/p)*(k/(p-1))  :: Double
        eHh_Hh  = (m/p) *((m-1)/(p-1)) :: Double
        eHh_hh  = (m/p) * (n/(p-1)) + (n/p)*(m/(p-1))
