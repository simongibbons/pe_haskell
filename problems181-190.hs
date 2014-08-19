import Math.NumberTheory.Primes.Factorisation
import Data.Ratio

-- Terminates function could do with speeding up
problem183 = sum $ map d $ [5..10000]
  where
    d n | terminates (maxDenominator n) = -n
        | otherwise                     = n

    terminates n | n == 1         = True
                 | n `mod` 5 == 0 = terminates (n `div` 5)
                 | n `mod` 2 == 0 = terminates (n `div` 2)
                 | otherwise      = False

    maxDenominator :: Integer -> Integer
    maxDenominator n = (denominator.maximum)  [ (n%k)^k | k<-[trialK, trialK + 1] ]
      where trialK = floor ((fromIntegral n) / e)

    e = exp 1 :: Double

problem187 = length $ filter (==2) $ map numFactors [1..limit]
  where
    limit = 10^8
    sieve = factorSieve limit
    numFactors n = sum $ map snd $ sieveFactor sieve n

problem188 = tetrate (10^8) 1777 1855
    where powerMod m b e = powm b e m 1 

          powm :: Integer -> Integer -> Integer -> Integer -> Integer
          powm b 0 m r = r
          powm b e m r | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
          powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

          tetrate m a 1 = a
          tetrate m a k = powerMod m a (tetrate m a (k-1))

-- Solution derived using Lagrange Multipliers
problem190 = sum [pm m | m <- [2..15] ]
  where pm m = floor $ product [(x1*i)**i | i <- [1..m] ]
          where x1 = ( 2 / (m+1) )

