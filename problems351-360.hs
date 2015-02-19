import Math.NumberTheory.Primes (primes, isPrime)
import Math.NumberTheory.Primes.Factorisation (divisors)
import qualified Data.Set as S


-- The only optimization here is to notice that the prime generating numbers
-- must be of the form (p-1) where p is a prime.
--
-- This could be optimized to only check up to sqrt n for the for the divisors.
problem357 = sum . filter isPGenInt $ candidates
  where isPGenInt x = all (\d -> isPrime (d + x `div` d)) (listDivisors x)
        listDivisors = S.toList . divisors
        candidates = map (subtract 1) . takeWhile (<10^8) $ primes

