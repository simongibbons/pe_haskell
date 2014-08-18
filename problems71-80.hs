import Data.Char
import Data.Ratio
import qualified Data.Set as Set
import Math.NumberTheory.Primes (totient)

problem71 = Set.findMax . Set.deleteMax $ fracs
  where
    fracs =  Set.fromList $ map nearestFrac [1..(10^6)]
    nearestFrac d = (3*d `div` 7) % d

-- Number of reduced proper fractions for denominator d <= 10^6
-- This is the length of the Farey sequence.
-- totient function is the number of numbers relitively prime to
-- a certain number.
problem72 = sum $ map totient [2..(10^6)]

problem73 = sum [1 | d<-[4..12000],
                     let lower = (d `div` 3) + 1,
                     let upper = (d `div` 2),
                     n <- [lower..upper],
                     gcd d n == 1 ]

--Probably Could optimize this a little more....
problem74 = length $ filter (==60) $ map (loopLength) [3..999999]
    where loopLength x
            | x == 169    = 3
            | x == 145    = 0
            | x == 2      = 0
            | x == 1      = 0
            | x == 40585  = 0
            | x == 363601 = 3
            | x == 1454   = 3
            | x == 871    = 2
            | x == 872    = 2
            | x == 45361  = 2
            | x == 45362  = 2
            | otherwise   = 1 + loopLength (facSum x)

          facSum x = sum $ map (fac . digitToInt) (show x)

          fac x = product [1..x]


{- Calculate the Partition function using Euler's recursion with
 - memoization. This is still pretty slow (takes around ~1h to run on
 - Core i5 Macbook). The issue is probably because I'm using a list
 - based strategy. Try using a map to speed this up.
 -}
problem78 = head $ filter (\x -> partitions x `mod` 10^6 == 0) [0..]
  where
    partitions = (map partitions' [0..] !!)
    partitions' n | n < 0 = 0
                  | n == 0 = 1
                  | otherwise = (sum pos) + (sum neg)
      where pos = map kthTerm $ takeWhile (\x -> fst x <= n) $ pent_pos
            neg = map kthTerm $ takeWhile (\x -> fst x <= n) $ pent_neg

            kthTerm (p,k) = (-1)^(abs (k-1)) * partitions (n - p )

            pent_pos = map (\k -> (k*(3*k-1) `div` 2, k) ) [1..]
            pent_neg = map (\k -> (k*(3*k-1) `div` 2, k) ) [-1,-2..]


problem79 = 73162890 --By Hand!

problem80 = sum $ [sqrtSum x | x<-[1..100], x `notElem` squareNos]
    where intSqrt' :: Integer -> Integer -> Integer -> Integer
          --Computes the sqrt of a large integer by bisection
          intSqrt' n lbound ubound = let guess = (lbound + ubound) `div` 2 in
                                         if abs( lbound - guess ) < 1
                                         then lbound
                                         else
                                             if guess*guess < n
                                             then intSqrt' n guess ubound
                                             else intSqrt' n lbound guess

          sqrtSum x = let m =(show $ intSqrt' (x*10^(202)) 0 (x*10^204))
                      in  sum $ map (digitToInt) $ take 100 m

          squareNos = takeWhile (<101) $ map (^2) [1..]
