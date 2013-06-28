import Data.Char (digitToInt)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.List

primesToLimit :: Integer -> [Integer]
primesToLimit m = 2 : sieve [3,5..m]
  where
    sieve (p:xs)
      | p*p > m = p : xs
      | True    = p : sieve [x | x <- xs, rem x p /= 0]


problem34 = sum [x | x<-[3..100000], isFacOfDig x]
    where isFacOfDig x = x == (sum $ map (fac . digitToInt) (show x))
          fac n = fac_list !! n
          fac_list = [1] ++ [ product [1..y] | y<-[1..9]]

problem37 = sum $ drop 4 [x | x<-(Set.toAscList primes), isRightTrunc x, isLeftTrunc x]
    where primes = Set.fromDistinctAscList $ primesToLimit 1000000
          isRightTrunc 0 = True
          isRightTrunc x = (x `Set.member` primes ) && ( isRightTrunc (x `div` 10) )

          isLeftTrunc 0 = True
          isLeftTrunc x = (x `Set.member` primes ) && ( isLeftTrunc (cutLeft x) )

          cutLeft x = x `mod` (10^((length.show $ x) - 1 ))


power10 x = floor ((log x) / (log 10))  

problem38 = maximum [read y :: Int | x<-[1..10000], y<-[makeNo x 1 [] ], isPan y ]
    where makeNo x n res
              | length (concat res) >= 9 = (concat res)
              | otherwise                = makeNo x (n+1) (res ++ [show (n*x)])

          isPan x = (sort x) == "123456789"

problem39 = maximumBy (comparing snd) [(p, numRightTris p) | p<-[12,14..1000]]
    where numRightTris p = length [1| c<-[1..(p`div`2)], b<-[1..(p-c-1)], a<-[(p-c-b)], a^2 + b^2 == c^2]

problem40 = product [ digitToInt (champVals !! (10^n)) | n<-[0..6] ]
    where champVals = foldr1 (++) (map show [0..])
