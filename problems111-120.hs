import Data.List (sort, elemIndex)
import Data.Maybe (fromJust)

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

