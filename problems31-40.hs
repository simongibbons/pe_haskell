import Data.Char (digitToInt)

problem34 = sum [x | x<-[3..100000], isFacOfDig x]
    where isFacOfDig x = x == (sum $ map (fac . digitToInt) (show x))
          fac n = fac_list !! n
          fac_list = [1] ++ [ product [1..y] | y<-[1..9]]


problem40 = product [ digitToInt (champVals !! (10^n)) | n<-[0..6] ]
    where champVals = foldr1 (++) (map show [0..])
