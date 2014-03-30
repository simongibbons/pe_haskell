import Math.NumberTheory.Primes.Factorisation (divisorPowerSum)
import Math.NumberTheory.Powers.Squares (isSquare)

-- Slow Ugly Bruteforce, revisit this at some point.
problem211 = sum $ map snd $ filter (isSquare.fst) $ map (\x -> (divisorPowerSum 2 x, x)) [1..64000000]
