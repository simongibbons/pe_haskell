import Math.NumberTheory.Primes (isPrime)

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
    rtHarshads = foldr (\x a -> rightTruncHarshad x ++ a) [] [1..13]

    strongRTHarshads :: [Integer]
    strongRTHarshads = filter (\x -> isPrime (x `div` (digSum x))) rtHarshads

    strongRTHarshadPrimes :: [Integer]
    strongRTHarshadPrimes = [10*x + i | x <- strongRTHarshads, i <- [1,3,7,9], isPrime (10*x + i)]

