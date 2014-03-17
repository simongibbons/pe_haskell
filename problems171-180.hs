import Math.NumberTheory.Primes.Factorisation (tau)
import Data.List (group)

problem179 = sum $ map (\x -> length x - 1) $ group $ map tau [1..(10^7)]
