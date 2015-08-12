import Math.NumberTheory.Primes (primes)

problem429 = sumSqUnitDivs . factorialFactors $ 100000000
  where
    factorialFactors n = map (\p -> (p, numTimes p)) ps
      where ps = takeWhile (<=n) primes
            numTimes p = sum . takeWhile (>0) $ [n `div` (p^k) | k <- [1..] ]

    m = 1000000009

    sumSqUnitDivs = foldl1 (\x y -> (x * y) `mod` m) . map (\(p,e) -> (p^(2*e) + 1) `mod` m )
