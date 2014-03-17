import Control.Monad (replicateM)
import Data.List (group, sort, genericLength)
import Data.Ratio

problem205 = ratioToDouble $ probBeat (probs 9 [1..4]) (probs 6 [1..6])
  where
    probBeat x y = sum $ [ (snd a * snd b) | a <- x, b <- y, (fst a) > (fst b) ]

    probs :: Int -> [Integer] -> [(Integer, Ratio Integer)]
    probs n dice = map makeProb . group . sort . map sum $ combinations
      where combinations = replicateM n dice
            makeProb x = (head x, (genericLength x) % (genericLength combinations) )

    ratioToDouble :: Integral a => Ratio a -> Double
    ratioToDouble x = ((fromIntegral.numerator) x) / ((fromIntegral.denominator) x)
