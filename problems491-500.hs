import Data.Ratio
import Math.NumberTheory.Primes (primes)
import qualified Data.Heap as H


-- For any colour the probability that it will be present is
-- P(Present) = 1 - P(Absent)
--            = 1 - (no of ways to pick 20 so that a colour is absent) /
--                  (no of ways to pick 20)
problem493 = fromRational $ 7 * (1 - (60 `choose` 20) % (70 `choose` 20))
  where n `choose` k = (product [n + 1 - i | i <- [1..k]]) `div` (product [1..k])


problem500 = productMod m costs
  where
    k = 500500
    m = 500500507

    productMod :: Integral a => a -> [a] -> a
    productMod m = foldr (\x acc -> (x*acc) `mod` m) 1

    primeHeap :: H.MinHeap Integer
    primeHeap = H.fromList . take k $ primes

    iterate' :: (a -> (a,b)) -> a -> [b]
    iterate' f a = b : (iterate' f a')
      where (a', b) = f a

    costs :: [Integer]
    costs = take k . iterate' next $ primeHeap
      where
        next heap = (heap', cost)
          where cost = head . H.take 1 $ heap
                heap' = H.insert (cost^2) (H.drop 1 heap)

