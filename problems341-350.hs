import qualified Data.Array.Unboxed as A
import Data.Algorithm.Munkres
import Math.NumberTheory.Primes (primes)

problem345 = do
    fin <- readFile "data/p345.dat"
    let matrix = (read fin) :: [Int]

    print $ matrixSum matrix
  where matrixSum :: [Int] -> Int
        matrixSum matrix = sum $ map (mArray A.!) elements
          where modifiedMatrix = A.listArray ((1,1),(15,15)) $ map (\x -> m - x) matrix
                    where m = maximum matrix

                mArray :: A.UArray (Int, Int) Int
                mArray = A.listArray ((1,1),(15,15)) matrix

                (elements,_) = hungarianMethodInt modifiedMatrix

problem347 = s 10000000
  where
    m :: Integer -> Integer -> Integer -> Integer
    m p q n | p*q > n = 0
            | otherwise = maximum .
                          takeWhile (\x -> (x `mod` p == 0) && (x <= n)) .
                          iterate next $ p^k*q
       where next :: Integer -> Integer
             next x = last . takeWhile (<=n) . iterate (*q) $ x `div` p
             k = floor $ (logInt n - logInt q) / logInt p
             logInt = log . fromIntegral

    s :: Integer -> Integer
    s n = sum $ s' n (takeWhile (<n `div` 2) primes)
      where s' n [] = []
            s' n (p:ps) = (sum . takeWhile (>0) $ [m p q n | q <- ps]) : s' n ps

