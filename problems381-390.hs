import Math.NumberTheory.Primes (isPrime, primeList, primeSieve)
import Math.NumberTheory.Moduli (powerModInteger)

problem381 = sum . map s . drop 2 . primeList . primeSieve $ 10^8
  where
    s :: Integer -> Integer
    s p = (sum . map (factorialMod p) $ [1..5] ) `mod` p

    -- Calculates the modular inverse for a **prime** modulus
    -- Uses Euler's thm. Which states that:
    --    a^(totient m) == 1 (mod m)
    -- and the fact that totient p = p - 1
    -- for prime p
    modInvPrime :: Integer -> Integer -> Integer
    modInvPrime a p = powerModInteger a (p - 2) p

    -- Calculates (p - k)! (mod p)
    -- Uses the fact that (p-1)! == -1 (mod p)
    -- and a fast modular inverse for prime moduli
    factorialMod :: Integer -> Integer -> Integer
    factorialMod p k = (-1 * (modInvPrime (product [p-k+1..p-1]) p)) `mod` p


-- 1) generate all right truncatable Harshad numbers
-- 2) find all strong right truncatable Harshads by filtering that list
-- 3) Try adding a digit to these to see if we form a prime
problem387 = sum strongRTHarshadPrimes
  where
    digSum :: Integer -> Integer
    digSum 0 = 0
    digSum n = m + digSum d
      where (d, m) = n `divMod` 10

    isHarshad :: Integer -> Bool
    isHarshad n = n `mod` (digSum n) == 0

    rightTruncHarshad :: Int -> [Integer]
    rightTruncHarshad 1 = [1..9]
    rightTruncHarshad n = [10*x + i | x <- rightTruncHarshad (n-1), i <- [0..9], isHarshad (10*x + i)]

    rtHarshads :: [Integer]
    rtHarshads = concatMap rightTruncHarshad [1..13]

    strongRTHarshads :: [Integer]
    strongRTHarshads = filter (\x -> isPrime (x `div` (digSum x))) rtHarshads

    strongRTHarshadPrimes :: [Integer]
    strongRTHarshadPrimes = [10*x + i | x <- strongRTHarshads, i <- [1,3,7,9], isPrime (10*x + i)]

