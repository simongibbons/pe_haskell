problem1 = sum [x| x<-[1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

problem2 = sum [x | x <-takeWhile (<4000000) $ map fib [1..], even x]
    where fib :: Int -> Integer
          fib = (map fib_base [0 ..] !!)
          fib_base 0 = 1
          fib_base 1 = 1
          fib_base n = fib (n-2) + fib (n-1)

problem4 = maximum $ [a*b| a<-[100..999], b<-[a..999], (reverse $ show (a*b) ) == (show (a*b))]

problem5 = foldr1 lcm [1..20]

problem6 = (sum [1..100])^2 - sum ( map (^2) [1..100] )

problem9 = product $ head [ [a,b,c] | c<-[1..1000], b<-[1..(1000-c-1)], a<-[1000-c-b], a^2 + b^2 == c^2 ]
