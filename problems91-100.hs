import Data.Ord (comparing)
import Data.List
import Data.Char

--Should Memoize This to improve the speed.
problem92 = length [1 | x<-[1..9999999], (endNo x) == 89 ]
    where
        endNo :: Int -> Int
        endNo 89 = 89
        endNo 1 = 1
        endNo n = endNo $ squareDig n

        squareDig :: Int -> Int
        squareDig n = sum $ map (^2) $ digits n

        digits = map (`mod` 10) . reverse . takeWhile (> 0) . iterate (`div` 10)


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
