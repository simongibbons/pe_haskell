import Data.List (sort, groupBy, sortBy, minimumBy, maximumBy)
import Math.NumberTheory.Primes.Factorisation (totient)
import Data.Ord (comparing)

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
