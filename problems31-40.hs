import Data.Char (digitToInt)

problem30 = sum $ [x | x<-[2..(6*9^5)], isFifthPowerSum x ]
    where isFifthPowerSum x = x == (sum $ map (fifthPower . digitToInt) (show x))
          fifthPower n = fifthPower_list !! n
          fifthPower_list = map (^5) [0..9]

problem34 = sum [x | x<-[3..100000], isFacOfDig x]
    where isFacOfDig x = x == (sum $ map (fac . digitToInt) (show x))
          fac n = fac_list !! n
          fac_list = [1] ++ [ product [1..y] | y<-[1..9]]
