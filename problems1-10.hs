import Data.Char

--Effectively Trial Division. This should be improved as solutions for 3, 7 and
--10 are pretty slow.
primesUnlimited = sieve [2..]
    where
        sieve (p:xs) = p : sieve [x | x <- xs, rem x p /= 0]

primesToLimit :: Integer -> [Integer]
primesToLimit m = 2 : sieve [3,5..m]
  where
    sieve (p:xs)
      | p*p > m = p : xs
      | True    = p : sieve [x | x <- xs, rem x p /= 0]

problem1 = sum [x| x<-[1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

problem2 = sum [x | x <-takeWhile (<4000000) $ map fib [1..], even x]
    where fib :: Int -> Integer
          fib = (map fib_base [0 ..] !!)
          fib_base 0 = 1
          fib_base 1 = 1
          fib_base n = fib (n-2) + fib (n-1)

problem3 = maximum $ primeFactors 600851475143
    where primeFactors n = [x| x<-(primesToLimit $ floor $ sqrt (fromIntegral n)), n `mod` x == 0]

problem4 = maximum $ [a*b| a<-[100..999], b<-[a..999], (reverse $ show (a*b) ) == (show (a*b))]

problem5 = foldr1 lcm [1..20]

problem6 = (sum [1..100])^2 - sum ( map (^2) [1..100] )

problem7 = primesToLimit 500000 !! 10000

problem8 = do
    inFile <- readFile "data/p8.dat"
    let number = read inFile :: Integer
    let productsList = findProducts (show number)
    print $ maximum productsList
    where
        findProducts :: String -> [Int]
        findProducts [] = []
        findProducts x = [product $ map digitToInt $ take 5 x ] ++ (findProducts (tail x))

problem9 = product $ head [ [a,b,c] | c<-[1..1000], b<-[1..(1000-c-1)], a<-[1000-c-b], a^2 + b^2 == c^2 ]

problem10 = sum $ primesToLimit 2000000
