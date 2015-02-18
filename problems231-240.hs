import Math.NumberTheory.Primes (primes)

-- Uses the formula for the exponent of the prime p in nCr found in
-- http://sriasat.files.wordpress.com/2012/12/eureka.pdf
problem231 = sumFactors 20000000 15000000
  where
    sumFactors n r = sum $ map (\x -> (fst x) * (snd x) ) $
                     zip pToCheck (map primeExponent pToCheck)
      where
        pToCheck = takeWhile (<n) primes

        primeExponent p = sum $ map summand $ takeWhile (\j -> p^j < n) [1..]
          where summand j = (n `div` p^j) - (r `div` p^j) - ( (n-r) `div` p^j )

-- Mathematica Code....
-- f[r_] := Sum[(900 - 3*k)*r^(k - 1), {k, 1, n}] /. n -> 5000
-- FindRoot[f[r] == -600000000000, {r, 1.001}, WorkingPrecision -> 13]
-- {r -> 1.002322108633}
problem235 =
    error "Done in Mathematica...... translate this to haskell at some point"
