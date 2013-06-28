import Data.Char

--Probably Could optimize this a little more....
problem74 = length $ filter (==60) $ map (loopLength) [3..999999]
    where loopLength x
            | x == 169    = 3
            | x == 145    = 0
            | x == 2      = 0
            | x == 1      = 0
            | x == 40585  = 0
            | x == 363601 = 3
            | x == 1454   = 3
            | x == 871    = 2
            | x == 872    = 2
            | x == 45361  = 2
            | x == 45362  = 2
            | otherwise   = 1 + loopLength (facSum x)

          facSum x = sum $ map (fac . digitToInt) (show x)

          fac x = product [1..x]

main = print problem74

problem79 = 73162890 --By Hand!
