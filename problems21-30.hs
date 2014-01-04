import Data.List
import Data.Char
import Data.Ord (comparing)
import Data.Maybe
import qualified Data.Set as Set

primesToLimit :: Integer -> [Integer]
primesToLimit m = 2 : sieve [3,5..m]
  where
    sieve (p:xs)
      | p*p > m = p : xs
      | True    = p : sieve [x | x <- xs, rem x p /= 0]


problem21 = sum $ filter hasPair [1..10000]
    where hasPair :: Int -> Bool
          hasPair x = (y /= d) && (x == y)
            where d = divs !! x
                  y = divs !! d

          divs :: [Int]
          divs = 0:(map sumPropDivs [1..])

          sumPropDivs :: Int -> Int
          sumPropDivs n = sum $ filter (\x -> mod n x == 0) [1..(n-1)]

problem22 = do
    inFile <- readFile "data/p22.dat"
    let nameVals = map sumName $ sort $ (read inFile :: [String])
    print $ sum [ (n+1) * (nameVals !! n) | n<-[0..(length nameVals - 1)]]
    where
        sumName name = sum $ map (\x -> (ord x) - 64) name

problem24 = (sort $ permutations [0,1,2,3,4,5,6,7,8,9] ) !! 999999

problem25 = head [x+1 | x<-[1..], (length $ show $ fib $ x) == 1000 ]
    where fib :: Int -> Integer
          fib = (map fib_base [0 ..] !!)
          fib_base 0 = 1
          fib_base 1 = 1
          fib_base n = fib (n-2) + fib (n-1)

-- Find the largest reccuring cycle in the decimal expansion of 1/d
-- for d < 1000
problem26 = fst $ maximumBy (comparing snd) $
            map (\x -> (x, cycleLength x)) [1..999]
  where
    cycleLength d = cycleLength' 1 d []

    cycleLength' n d rems | m == 0    = 0
                          | isJust i  = (fromJust i) + 1
                          | otherwise = cycleLength' (10*m) d (m:rems)
      where m = n `mod` d
            i = findIndex (==m) rems --Slow because of the findIndex call

problem27 = maximumBy (comparing snd) [(a*b, numPrimes a b) | a<-[-1000..1000], b<-(primesToLimit 1000)]
    where numPrimes a b = length $ takeWhile (\x -> Set.member x primes) $ map (\x -> x^2 + a*x + b) [0..]
          primes = Set.fromDistinctAscList $ primesToLimit 1000000

problem29 = length $ nub $ [a^b| a<-[2..100], b<-[2..100] ]

problem30 = sum $ [x | x<-[2..(6*9^5)], isFifthPowerSum x ]
    where isFifthPowerSum x = x == (sum $ map (fifthPower . digitToInt) (show x))
          fifthPower n = fifthPower_list !! n
          fifthPower_list = map (^5) [0..9]



