import Data.Ord (comparing)
import Data.List

problem99 = do
    inList <- readFile "p99.dat"
    let baseExpList = createIntTuples inList
    --Find The Maximum Value
    let maxval = fst $ maximumBy (comparing snd) [(x, ((snd x) * log(fst x)) ) | x<-baseExpList]
    print $ findIndex maxval baseExpList + 1
        where
            createIntTuples :: String -> [(Double, Double)]
            createIntTuples input = map read $ lines input
            findIndex val x = head ( filter ( (==val) . (x!!) ) [0..((length x) - 1)] )
