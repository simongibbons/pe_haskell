import Data.Char
import Data.List
import Data.Text (splitOn)
import Data.Ord (comparing)
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

problem13 = do
    inList <- readFile "data/p13.dat"
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

problem17 = sum $ map ( length . letters ) [1..1000]
    where
        ones = ["one","two","three","four","five","six","seven","eight",
                "nine","ten","eleven","twelve","thirteen","fourteen","fifteen",
                "sixteen","seventeen","eighteen", "nineteen"]
        tys  = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

        letters x
            | x == 0        = []

            | x < 20        = ones !! (x-1)

            | x < 100       = tys !! ( (x `div` 10) - 2 ) ++
                              letters (x - (x `div` 10) * 10)

            | x < 1000 && (x `mod` 100) == 0
                            = ones !! ( (x `div` 100 ) -1 ) ++ "hundred"

            | x > 100 && x <= 999
                            = ones !! ( (x `div` 100 ) -1 ) ++ "hundredand" ++
                                letters (x - (x `div` 100) * 100)

            | x == 1000     = "onethousand"

problem18 = do
    inFile <- readFile "data/p18.dat"
    print $ maxPathSum (read inFile :: [[Int]])
    where
        maxPathSum = head . foldr1 step
        step [] [z] = [z]
        step (x:xs) (y:z:zs) = x + max y z : step xs (z:zs)

problem19 = length [() | y <- [1901..2000], 
                             m <- [1..12],
                             let (_, _, d) = toWeekDate $ fromGregorian y m 1,
                             d == 7]

problem20 = facSum 100
    where facSum n = sum $ map digitToInt $ show (fac n)
          fac n    = product [1..n]
