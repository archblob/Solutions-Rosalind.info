module ExpectedValue (
  expectedValue
  ) where

expectedValue:: Int -> Int -> Float -> Float
expectedValue m n gc = (fromIntegral (n - m + 1))*((pA^2 + pC^2 + pG^2 + pT^2)^m)
  where
    pC = gc / 2 :: Float
    pG = pC
    pA = (1.0 - gc) / 2 :: Float
    pT = pA
