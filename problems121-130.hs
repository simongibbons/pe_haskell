import Math.NumberTheory.Primes (factorise, primes)
import Data.List (sortBy)
import Data.Ord (comparing)

problem123 = head $ dropWhile (\x -> fst x < 10^10) $
             map (\x -> (sqRemainder x, fst x) ) $ zip [1..] primes
  where sqRemainder (n, pn) = ((pn - 1)^n + (pn + 1)^n) `mod` pn^2

problem124 = ( sortBy (comparing snd) rads ) !! 9999
  where rads = map (\x -> (x, radical x) ) [1..100000]
        radical n = product $ map fst (factorise n)
