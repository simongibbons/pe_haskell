import Data.Char
import System.IO
import Data.List
import Data.Text (splitOn)
import Data.Ord (comparing)

problem13 = do
    inList <- readFile "p13.dat"
    print $ take 10 $ show $ sum $ createIntList inList
        where
            createIntList :: String -> [Integer]
            createIntList input = map read $ lines input

problem14 = maximumBy (comparing snd) [(x, chainLength x) | x<-[1..1000000] ]
    where chainLength :: Int -> Int
          chainLength 1 = 1
          chainLength n
                    | even n = 1 + chainLength ( n `div` 2)
                    | odd n  = 1 + chainLength (3*n + 1)

problem15 = choose 40 20
    where choose n k = product [(k+1)..n] `div` product [1..k]

problem16 = power2sum 1000
    where power2sum n = sum $ map digitToInt $ show (2^n)

problem20 = facSum 100
    where facSum n = sum $ map digitToInt $ show (fac n)
          fac n    = product [1..n]
