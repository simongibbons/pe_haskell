import Data.Char
import qualified Data.Set as Set
import Math.NumberTheory.Primes.Sieve (primes)
import Math.NumberTheory.Primes.Factorisation (factorise)
import Data.List (groupBy, sortBy, sort, group, maximumBy)
import Data.Ord (comparing)
import qualified Data.Array as A

primesToLimit :: Integer -> [Integer]
primesToLimit m = 2 : sieve [3,5..m]
  where
    sieve (p:xs)
      | p*p > m = p : xs
      | True    = p : sieve [x | x <- xs, rem x p /= 0]



problem41 = maximum $ filter isPan $ takeWhile (<10^8) primes
  where
    isPan :: Integer -> Bool
    isPan x | l == 1    = s == "1"
            | l == 2    = s == "12"
            | l == 3    = s == "123"
            | l == 4    = s == "1234"
            | l == 5    = s == "12345"
            | l == 6    = s == "123456"
            | l == 7    = s == "1234567"
            | otherwise = False
        where s = (sort.show) x
              l = length s

problem42 = do
    fileIn <- readFile "data/p42.dat"
    let wordSums = map sumWord (read fileIn :: [String])
    let triNos = triangularToN $ maximum wordSums
    print $ length $ filter (\x -> x `elem` triNos) wordSums
    where
        sumWord word = sum $ map (\x -> (ord x) - 64) word
        triangularToN n = takeWhile (<n) $ map (\x -> x*(x+1) `div` 2) [1..]

problem44 = head [x-y | x<-pents, y<-takeWhile (<x) pents, isPent (x+y), isPent (x-y)]
    where isPent n =
            let (ai, af) = properFraction . sqrt $ 1 + 24 * (fromInteger n)
            in  (af == 0) && ai `mod` 6 == 5

          pents = [n*(3*n-1) `div` 2 | n<-[1..2500] ]

problem45 = head [x | x <- scanl (+) 1 [5,9..], x > 40755, isPent x]
    where isPent n =
            let (ai, af) = properFraction . sqrt $ 1 + 24 * (fromInteger n)
            in  (af == 0) && ai `mod` 6 == 5

problem46 = head $ filter (isNotPrime) $ filter (isCounterExample) [3,5..]
    where primes = Set.fromDistinctAscList $ primesToLimit 10000
          isPrime x = (x `Set.member` primes)
          isNotPrime x = (x `Set.notMember` primes )
          isCounterExample x = isCounterExample' x 1
          isCounterExample' x n
                | (x - (2*n^2)) < 2     = True
                | isPrime (x - (2*n^2)) = False
                | otherwise             = isCounterExample' x (n+1)

problem47 = ((map snd).head) $
            filter (\x -> length x >= nConsec && (fst.head) x == nFactors) $
            groupBy (\x y -> fst x == fst y) $
            (map (\x -> (numPrimeFactors x, x) ) [1..])
    where
        numPrimeFactors :: Integer -> Int
        numPrimeFactors = (length.factorise)

        nFactors = 4
        nConsec  = 4

problem48 = ( sum $ [x^x `mod` (10^10) | x<-[1..1000] ] ) `mod` (10^10)

problem49 = concatMap findGroups $ primePerms
  where findGroups :: [Integer] -> [(Integer, Integer, Integer)]
        findGroups x = [(a,b,2*b-a)| a <- x,
                                     b <- dropWhile (<=a) x,
                                     (2*b - a) `elem` x ]

        primePerms = filter (\x -> length x >= 3) $ findPerms primeList

        findPerms :: [Integer] -> [[Integer]]
        findPerms x = permList
          where sortedList  = sortBy (comparing fst) $ zip ( map (sort.show) x ) x
                groupedList = groupBy (\p q -> fst p == fst q) sortedList
                permList    = map (map snd) groupedList

        primeList = takeWhile (<10000) $ dropWhile (<1000) primes

problem50 = snd $ maximumBy (comparing fst) $ consecSums
  where
    consecSums = [(n, p) | i <- [1..l],
                           j <- [1..i],
                           let p = (primeSumArray A.! i) - (primeSumArray A.! j ),
                           isPrime p,
                           let n = i - j
                 ]

    limit = 10^6
    l = length primeSums

    primesToLimit = takeWhile (<limit) primes

    primeSet = Set.fromDistinctAscList primesToLimit
    isPrime = (flip Set.member) primeSet

    primeSums = takeWhile (<limit) $ scanl1 (+) primesToLimit
    primeSumArray = A.listArray (1, l) primeSums
