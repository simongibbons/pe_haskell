import Math.NumberTheory.Primes (factorise, primes)
import Data.List (sortBy, nub)
import Data.Ord (comparing)

--Could probably memoize the recursion for calculating ways but it seems
--to run fast enough as it is.
problem121 = prizeFund 15
  where
    prizeFund :: Integer -> Integer
    prizeFund nTurns = (nOutcomes nTurns) `div` (nWins nTurns)

    nWins :: Integer -> Integer
    nWins nTurns = sum $ map (ways nTurns) $ [(nTurns `div` 2 + 1)..nTurns]

    nOutcomes :: Integer -> Integer
    nOutcomes nTurns = sum $ map (ways nTurns) $ [0..nTurns]

    ways :: Integer -> Integer -> Integer
    --Gives the number of ways of getting nblues
    --from a game with n turns.
    ways 1 0 = 1
    ways 1 1 = 1
    ways nTurns nBlues | nBlues > nTurns = 0
                       | nBlues < 0      = 0
                       | otherwise       = nTurns*(ways (nTurns - 1) nBlues ) + --gained a red
                                           (ways (nTurns - 1) (nBlues - 1) )    --gained a blue

problem123 = head $ dropWhile (\x -> fst x < 10^10) $
             map (\x -> (sqRemainder x, fst x) ) $ zip [1..] primes
  where sqRemainder (n, pn) = ((pn - 1)^n + (pn + 1)^n) `mod` pn^2

problem124 = ( sortBy (comparing snd) rads ) !! 9999
  where rads = map (\x -> (x, radical x) ) [1..100000]
        radical n = product $ map fst (factorise n)

problem125 = sum $ nub $ filter isPal $ concatMap sumSqNos [1..7071]
  where isPal x = show x == (reverse.show) x
        sumSqNos n = takeWhile (<10^8) $ tail $ scanl1 (+) $ map (^2) [n..]
