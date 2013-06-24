import Data.Char (digitToInt)

problem56 = maximum $ [digitSum (a^b)| a<-[1..99], b<-[1..99] ]
    where digitSum n = sum $ map (digitToInt) (show n)
