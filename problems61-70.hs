import Data.List (sort, groupBy, sortBy, minimumBy, maximumBy)
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

problem67 = do
    inFile <- readFile "data/p67.dat"
    print $ maxPathSum (read inFile :: [[Int]])
    where
        maxPathSum = head . foldr1 step
        step [] [z] = [z]
        step (x:xs) (y:z:zs) = x + max y z : step xs (z:zs)

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

