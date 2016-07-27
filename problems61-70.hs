import Data.List (sort, groupBy, sortBy, minimumBy, maximumBy, permutations)
import Math.NumberTheory.Primes.Factorisation (totient)
import Data.Ord (comparing)
import Data.Ratio
import Data.Char (digitToInt)

problem62 = snd $ minimumBy (comparing snd) $
            concat.(filter (\x -> length x == 5)) $
            groupBy (\x y -> fst x == fst y) $
            sortedList 12
    where
      cubeList n = zip (map (sort.show) cubes) cubes
        where cubes = takeWhile (<10^n) $ dropWhile (<10^(n-1)) $ map (^3) [1..]

      sortedList x = sortBy (comparing fst) $ cubeList x


problem63 = sum $ takeWhile (\x -> numNdigitNthPower x > 0) [numNdigitNthPower x | x<-[1..]]
    where
        getPowerNos n = takeWhile (\x -> (length $ show (x^n)) <= n) [1..]
        numNdigitNthPower n = length $ filter (\x -> ((length $ show (x^n)) == n) ) $ getPowerNos n

-- How Many squareroots of n <= 10000 have continued fraction representations 
-- of odd periods.
problem64 = length $ filter (\x -> (length x) `mod` 2 == 0 ) $ map cfSqrt [1..10000]
  where
    --Determine the Continued Fraction Representation of a square root
    cfSqrt :: Int -> [Int]
    cfSqrt n | isSquare n = [floor $ sqrt $ (fromIntegral n::Double)]
             | otherwise  = cfSqrt' n 0 1
      where
        cfSqrt' :: Int -> Int -> Int -> [Int]
        cfSqrt' r n d = m : end
          where
            m = (truncate (sqrt (fromIntegral r)) + n) `div` d
            a = d * m - n

            --Detect if we are at the end of a repeating cycle
            end | d == 1 && n /= 0 = []
                | otherwise        = cfSqrt' r a ((r - a ^ 2) `div` d)

        isSquare :: Int -> Bool
        isSquare n = sq * sq == n
          where sq = floor $ sqrt $ (fromIntegral n::Double)

-- Find the Sum of the Digits in the Numerator of the 100th convergent of e
problem65 = sum $ map digitToInt . show $ numerator $ frac ( take 100 eFracRep )
  where
    frac :: Integral a => [a] -> Ratio a
    frac (x:[]) = x % 1
    frac (x:xs) = (x % 1) + (1 / (frac xs))

    eFracRep = 2:(map eFracRep' [0..])
    eFracRep' n | (n `mod` 3 - 1) == 0 = 2 * ( n `div` 3 + 1)
                | otherwise            = 1

-- Find the minimum solution to Pell's equation for D <= 1000
-- Uses the method of continued fractions as found on the
-- wikipedia page
problem66 = maximumBy (comparing snd) $
            map (\x -> (x, minPellSolution x) ) $
            filter (isNotSquare) [2..1000]
  where
    isPellSolution :: Integer -> Integer -> Integer -> Bool
    isPellSolution x y d = x^2 - d*y^2 == 1

    frac :: Integral a => [a] -> Ratio a
    frac (x:[]) = x % 1
    frac (x:xs) = (x % 1) + (1 / (frac xs))

    cfSqrtList :: Integer -> [[Integer]]
    cfSqrtList n = map (\x -> take x (cfSqrt' n 0 1) ) [1..]

    minPellSolution :: Integer -> Integer
    minPellSolution d = numerator $ head $
                        filter (\x -> isPellSolution (numerator x) (denominator x) d ) $
                        sqrtConvergents d

    sqrtConvergents :: Integer -> [Ratio Integer]
    sqrtConvergents n = map frac (cfSqrtList n)

    cfSqrt' :: Integer -> Integer -> Integer -> [Integer]
    cfSqrt' r n d = m : cfSqrt' r a ((r - a ^ 2) `div` d)
      where
        m = (truncate (sqrt (fromIntegral r)) + n) `div` d
        a = d * m - n

    isNotSquare :: Integer -> Bool
    isNotSquare n = sq * sq /= n
      where sq = floor $ sqrt $ (fromIntegral n::Double)

problem67 = do
    inFile <- readFile "data/p67.dat"
    print $ maxPathSum (read inFile :: [[Int]])
    where
        maxPathSum = head . foldr1 step
        step [] [z] = [z]
        step (x:xs) (y:z:zs) = x + max y z : step xs (z:zs)

problem68 = maximumBy (comparing stringToInteger) . map makeString $ candidates
  where
    makeString p = concat [show (p!!i) | i <- [0,1,2,3,2,4,5,4,6,7,6,8,9,8,1]]

    candidates = filter isCandidate . permutations $ [1..10]

    isCandidate p = and [tenInOuter p, zeroSmallest p, sameSum p]
      where
        tenInOuter p = and [ p !! i /= 10 | i <- [1,2,4,6,8] ]

        zeroSmallest p = and [ p !! 0 < p !! i | i <- [3,5,7,9] ]

        sameSum p = and [row == i | i <- rows]
          where
            rowSum idxs = sum [p!!i | i <- idxs]
            (row:rows) = map rowSum [[0,1,2], [3,2,4], [5,4,6], [7,6,8], [9,8,1]]

    stringToInteger :: String -> Integer
    stringToInteger = read

problem69 = snd $ maximumBy (comparing fst) $ map (\x -> (f x, x)) [1..1000000]
  where
     f :: Integer -> Double
     f n = (fromIntegral n) / ( fromIntegral (totient n))

--Very Slow (runs in ~50s) Needs improving at some point.
problem70 = fst $ minimumBy (comparing snd) $ map (\x -> (fst x, ratio x)) permList
  where
    permList = filter (\(x,y) -> isPerm x y) $ zip [2..10^7] (map totient [2..10^7])

    isPerm x y = ((sort.show) x) == ((sort.show) y)

    ratio (x,y) = (fromIntegral x) / (fromIntegral y)

