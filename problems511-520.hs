import Math.NumberTheory.Primes (totientSieve, sieveTotient)


problem512 = g (5*10^8)
  where g n = sum . map f $ [1,3..n]
          where sieve = totientSieve n
                f x = (sieveTotient sieve x) `mod` (n+1)

main = print problem512
