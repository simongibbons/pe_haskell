import Math.NumberTheory.Primes (factorise)
import Data.List (sortBy)
import Data.Ord (comparing)


problem124 = ( sortBy (comparing snd) rads ) !! 9999
  where rads = map (\x -> (x, radical x) ) [1..100000]
        radical n = product $ map fst (factorise n)
