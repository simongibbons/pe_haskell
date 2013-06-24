import Data.List
import Data.Char

problem25 = head [x+1 | x<-[1..], (length $ show $ fib $ x) == 1000 ]
    where fib :: Int -> Integer
          fib = (map fib_base [0 ..] !!)
          fib_base 0 = 1
          fib_base 1 = 1
          fib_base n = fib (n-2) + fib (n-1)

problem29 = length $ nub $ [a^b| a<-[2..100], b<-[2..100] ]

problem30 = sum $ [x | x<-[2..(6*9^5)], isFifthPowerSum x ]
    where isFifthPowerSum x = x == (sum $ map (fifthPower . digitToInt) (show x))
          fifthPower n = fifthPower_list !! n
          fifthPower_list = map (^5) [0..9]
