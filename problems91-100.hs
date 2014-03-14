import Data.Ord (comparing)
import Data.List
import Data.Char
import Math.NumberTheory.Primes.Factorisation (divisorSum)
import qualified Data.Map as Map
import Data.Maybe (isNothing, fromJust)

problem92 = length . filter (==89) . map endNo $ [1..9999999]
    where
        endNo :: Int -> Int
        endNo n = endNo_precalc !! (squareDig n)

        endNo_precalc :: [Int]
        endNo_precalc = [endNo_recurse x | x<-[0..567]]

        endNo_recurse :: Int -> Int
        endNo_recurse n
                | n == 89   = 89
                | n == 1    = 1
                | n == 0    = 0
                | otherwise = endNo_recurse $ squareDig n

        squareDig :: Int -> Int
        squareDig = sum . map ( (^2) . digitToInt ) . show

-- Find the smallest element of an amicable chain with elements
-- not exceeding 10^6
problem95 = snd $ chains !! (fromJust (elemIndex maxChain chains))
  where
    chains = map chain [1..10^6]
    maxChain = maximum chains

    propDivMap = Map.fromAscList $
                 map (\x -> (Just (fromIntegral x),
                             fromIntegral((divisorSum x) - x))) $ [1..10^6]

    chain x = chain' [Just x]

    chain' :: [Maybe Int] -> (Int, Maybe Int)
    chain' l | elem d l    = ((fromJust (elemIndex d l) + 1),
                              minimum (d:(takeWhile (/= d) l)))
             | isNothing d = (0, Nothing)
             | otherwise   = chain' (d:l)
        where d = Map.lookup (head l) propDivMap
              i = elemIndex d l

problem97 = (28433 * 2^7830457 + 1) `mod` (10^10)

problem99 = do
    inList <- readFile "data/p99.dat"
    let baseExpList = createIntTuples inList
    --Find The Maximum Value
    let maxval = fst $ maximumBy (comparing snd) [(x, ((snd x) * log(fst x)) ) | x<-baseExpList]
    print $ findIndex maxval baseExpList + 1
        where
            createIntTuples :: String -> [(Double, Double)]
            createIntTuples input = map read $ lines input
            findIndex val x = head ( filter ( (==val) . (x!!) ) [0..((length x) - 1)] )

-- Uses the solution to the diophontine equation implied by the problem
problem100 = findSolution 15 21
  where findSolution b n | n >= 10^12 = b
                         | otherwise  = findSolution (3*b + 2*n - 2) (4*b + 3*n -3)
