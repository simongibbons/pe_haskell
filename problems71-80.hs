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

problem79 = 73162890 --By Hand!

problem80 = sum $ [sqrtSum x | x<-[1..100], x `notElem` squareNos]
    where intSqrt' :: Integer -> Integer -> Integer -> Integer
          --Computes the sqrt of a large integer by bisection
          intSqrt' n lbound ubound = let guess = (lbound + ubound) `div` 2 in
                                         if abs( lbound - guess ) < 1
                                         then lbound
                                         else
                                             if guess*guess < n
                                             then intSqrt' n guess ubound
                                             else intSqrt' n lbound guess

          sqrtSum x = let m =(show $ intSqrt' (x*10^(202)) 0 (10^404))
                      in  sum $ map (digitToInt) $ [m!!n | n<-[0..99]]

          squareNos = takeWhile (<101) $ map (^2) [1..]
