import Data.Char (digitToInt)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.List

primesToLimit :: Int -> [Int]
primesToLimit m = 2 : sieve [3,5..m]
  where
    sieve (p:xs)
      | p*p > m = p : xs
      | True    = p : sieve [x | x <- xs, rem x p /= 0]

-- This is really not optimal but it works!
problem32 = sum $ nub $ [x*y| x<-[1..5000], y<-[x..5000], isPan x y ]
    where isPan x y = (sort $ concat $ map show [x,y,x*y]) == "123456789"

problem34 = sum [x | x<-[3..100000], isFacOfDig x]
    where isFacOfDig x = x == (sum $ map (fac . digitToInt) (show x))
          fac n = fac_list !! n
          fac_list = [1] ++ [ product [1..y] | y<-[1..9]]

problem35 = length $ filter (isCircPrime) $ takeWhile (<1000000) (Set.toAscList primes)
    where primes = Set.fromDistinctAscList $ primesToLimit 1000000

          isCircPrime x = ( foldr1 (&&) $ map (`Set.member` primes) $ digitRotations x ) &&
                          ( '0' `notElem` (show x) )

          digitRotations x = digitRotations_base x (length.show $ x) []

          digitRotations_base x n res
                    | n == 0    = res
                    | otherwise = let y = rotateDigits x
                                  in digitRotations_base y (n-1) (y:res)

          rotateDigits :: Int -> Int
          rotateDigits x = read $ (tail (show x) ) ++ [ head ( show x ) ]

problem36 = sum $ filter (\x -> (isPal (show x) ) && (isPal (toBin x) ) ) [1..1000000]
    where toBin 0 = []
          toBin n = reverse $ helper n

          helper 0 = []
          helper n = let (q,r) = n `divMod` 2 in r : helper q

          isPal s = s == (reverse s)

problem37 = sum $ drop 4 [x | x<-(Set.toAscList primes), isRightTrunc x, isLeftTrunc x]
    where primes = Set.fromDistinctAscList $ primesToLimit 1000000
          isRightTrunc 0 = True
          isRightTrunc x = (x `Set.member` primes ) && ( isRightTrunc (x `div` 10) )

          isLeftTrunc 0 = True
          isLeftTrunc x = (x `Set.member` primes ) && ( isLeftTrunc (cutLeft x) )

          cutLeft x = x `mod` (10^((length.show $ x) - 1 ))


problem38 = maximum [read y :: Int | x<-[1..10000], y<-[makeNo x 1 [] ], isPan y ]
    where makeNo x n res
              | length (concat res) >= 9 = (concat res)
              | otherwise                = makeNo x (n+1) (res ++ [show (n*x)])

          isPan x = (sort x) == "123456789"

problem39 = maximumBy (comparing snd) [(p, numRightTris p) | p<-[12,14..1000]]
    where numRightTris p = length [1| c<-[1..(p`div`2)], b<-[1..(p-c-1)], a<-[(p-c-b)], a^2 + b^2 == c^2]

problem40 = product [ digitToInt (champVals !! (10^n)) | n<-[0..6] ]
    where champVals = foldr1 (++) (map show [0..])
