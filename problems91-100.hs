import Data.Ord (comparing)
import Data.List
import Data.Char
import Math.NumberTheory.Primes.Factorisation (divisorSum)
import qualified Data.Map as Map
import Data.Maybe (isNothing, fromJust)

-- Lazy BruteForce checking all possible triangles
problem91 = length $
            [1 | x1 <- [0..l], x2 <- [0..l], y1 <- [0..l], y2 <- [0..l],
                 let p = (x1,y1), let q = (x2, y2),
                 (y2 - x2) < (y1 - x1),
                 p /= q, p /= (0,0), q /= (0,0),
                 isRightTriangle p q ]
  where l = 50
        isRightTriangle (x1, y1) (x2, y2) = (s!!0) + (s!!1) == (s!!2)
          where s = sort $ [ x1^2 + y1^2, x2^2 + y2^2, (x1-x2)^2 + (y1-y2)^2]

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

type Mapping = [Int]

-- TODO despegettify this code and generalise the mapping function to allow
-- for more than just strings with all distinct elements
problem98 = do
    fin <- readFile "data/p98.dat"
    let allWords = read $ "[" ++ fin ++ "]" :: [String]
    let anagrams = findAnagrams $ allWords
    let mappings = map makeMapping anagrams
    let trials  = map makeTrials mappings
    print $ maximum $ resToInts $ concatMap matchOnFst $ zip mappings trials
  where
    findAnagrams :: (Eq a, Ord a) => [[a]] -> [([a],[a])]
    findAnagrams l = concatMap makePairs $ map (map fst) $ groupedPairs
      where
        pairs = map (\x -> (x, sort x) ) l
        groupedPairs = groupBy (\x y -> snd x == snd y) $ sortBy (comparing snd) pairs

    makePairs :: [a] -> [(a,a)]
    makePairs xs = [(y,ys) | y:xs' <- tails xs
                           , ys <- xs']

    makeMapping :: Eq a => ([a],[a]) -> Mapping
    makeMapping ([],_) = []
    makeMapping ((x:xs),y) = ((fromJust.elemIndex x) y) : makeMapping (xs,y)

    squares :: Int -> [Int]
    squares n = takeWhile (<10^n) $ dropWhile (<10^(n-1)) $ map (^2) [1..]

    findSqAnagrams :: Int -> [(String, String)]
    findSqAnagrams n = findAnagrams $ map show $ squares n

    matchOnFst :: Eq a => (a, [(a,b)]) -> [b]
    matchOnFst (e,l) = map snd $ filter (\x -> (fst x) == e) l

    makeTrials m = map (\x -> (makeMapping x, (fst x, snd x)) ) $ findSqAnagrams (length m)

    resToInts :: [(String,String)] -> [Int]
    resToInts [] = []
    resToInts ((a,b):xs) = [read a] ++ [read b] ++ resToInts xs

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
