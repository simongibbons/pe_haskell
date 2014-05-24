import Math.NumberTheory.Primes.Factorisation (divisorPowerSum, totientSieve, sieveTotient)
import Math.NumberTheory.Primes (primes)
import Math.NumberTheory.Powers.Squares (isSquare)

-- Slow Ugly Bruteforce, revisit this at some point.
problem211 = sum $ map snd $ filter (isSquare.fst) $ map (\x -> (divisorPowerSum 2 x, x)) [1..64000000]

problem214 = sum $ filter (\x -> (chainLength) x == 25) $ takeWhile (<limit) primes
  where
    limit = 40000000

    chainLength 1 = 1
    chainLength n = 1 + chainLength (totient' n)

    totient' = sieveTotient sieve
    sieve = totientSieve limit

