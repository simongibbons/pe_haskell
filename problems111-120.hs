import Data.List (sort)

problem119 = (sort [a^b| a<-[2..100], b<-[2..9], sumDig (a^b) == a])!!29
  where
    sumDig :: Int -> Int
    sumDig 0 = 0
    sumDig n = (n `rem` 10) + sumDig (n `div` 10)

