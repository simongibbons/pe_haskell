import Data.Ord (comparing)
import Data.List
import Data.Char

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
