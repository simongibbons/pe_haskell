import Math.NumberTheory.Primes (divisorCount)

-- This is horrendusly slow..... improve it by using a Deque data structure
-- to cache the current sequence rather than having to recalculate it from
-- scratch quite often.
problem485 = sum $ map snd $ take numTerms $ iterate (nextVal k) firstTerm
  where numTerms = fromIntegral (u-k+1)
        firstTerm = (1, calcM 1 k)

        nextVal :: Integer -> (Integer,Integer) -> (Integer,Integer)
        nextVal k (prevN, prevVal) | prevVal == (divisorCount prevN) = (nextN, calcM nextN k)
                                   | otherwise                       = (nextN, max prevVal (divisorCount (prevN + k)) )
          where nextN = prevN + 1

        calcM :: Integer -> Integer -> Integer
        calcM n k = maximum $ map divisorCount [n..(n+k-1)]

        u = 100000000
        k = 100000

