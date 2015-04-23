import Control.Monad (replicateM)
import Data.List (group, sort, genericLength, nub)
import Math.NumberTheory.Primes (primes)
import Data.Ratio

-- Uses a quick way to find the power of any prime in the factorisation of nCr
-- (P231)
problem203 = sum $ nub [nCr n r | n<-[0..50], r<-[0..n], isSquareFree n r]
  where
    nCr n r = (product [(n-r+1)..n]) `div` (product [1..r])

    isSquareFree n r = null $ filter (>=2) $ exponents n r
      where
        exponents n r = map primeExponent pToCheck

        pToCheck = takeWhile (<=n) primes

        primeExponent p = sum $ map summand $ takeWhile (\j -> p^j <= n) [1..]
          where summand j = (n `div` p^j) - (r `div` p^j) - ( (n-r) `div` p^j )

problem205 = ratioToDouble $ probBeat (probs 9 [1..4]) (probs 6 [1..6])
  where
    probBeat x y = sum $ [ (snd a * snd b) | a <- x, b <- y, (fst a) > (fst b) ]

    probs :: Int -> [Integer] -> [(Integer, Ratio Integer)]
    probs n dice = map makeProb . group . sort . map sum $ combinations
      where combinations = replicateM n dice
            makeProb x = (head x, (genericLength x) % (genericLength combinations) )

    ratioToDouble :: Integral a => Ratio a -> Double
    ratioToDouble x = ((fromIntegral.numerator) x) / ((fromIntegral.denominator) x)

-- Any square ending in 0 must also end in 00, we can form a lower bound
-- on the possible square root and brute force from there.... not a fast
-- or pretty solution though!
problem206 = (*10) $ head [ x | x <- [100000000..], hasProperty (x^2) ]
  where every :: Int -> [a] -> [a]
        every _ [] = []
        every n (x:xs) = x : every n (drop (n-1) xs)

        hasProperty :: Integer -> Bool
        hasProperty n = ((every 2 . show) n) == ['1'..'9']

problem207 = head $ dropWhile (\(_,r) -> r >= (1%12345)) pMs
  where pMs = [(m, numPerfectLT m % n) | (m,n) <- zip (tail partitions) [2..]]
        
        numPerfectLT m = fromIntegral $ length (takeWhile (<=m) perfectPartitions)

        perfectPartitions = [4^t - 2^t | t <- [1..] ]
        partitions = [m^2 - m | m <- [2..]]
