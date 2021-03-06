import Data.List (sort, elemIndex)
import Data.Maybe (fromJust)

problem113 = nonBouncy 100
 where
    nonBouncy n  = (increasing n) + (decreasing n) - (doubles n)
    increasing n = (choose (n + 9) 9) - 1
    decreasing n = (choose (n + 10) 10) - 1 - n
    doubles n    = 9*n

    choose n k = (factorial n) `div` ((factorial k) * (factorial (n-k)))
    factorial n = product [1..n]


problem119 = (sort [a^b| a<-[2..100], b<-[2..9], sumDig (a^b) == a])!!29
  where
    sumDig :: Int -> Int
    sumDig 0 = 0
    sumDig n = (n `rem` 10) + sumDig (n `div` 10)

problem120 = sum $ map maxRemainder [3..1000]
  where
    maxRemainder a = maximum $ map (remainder a) [1..p]
      where p = period a

    period a = fromJust $ elemIndex (head l) (tail l)
      where
        l = map (remainder a) [1..]

    remainder a n = ( (a-1)^n + (a+1)^n ) `mod` a^2

