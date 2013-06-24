problem1 = sum [x| x<-[1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

problem2 = sum [x | x <-takeWhile (<4000000) $ map fib [1..], even x]
    where fib :: Int -> Integer
          fib = (map fib_base [0 ..] !!)
          fib_base 0 = 1
          fib_base 1 = 1
          fib_base n = fib (n-2) + fib (n-1)

problem6 = (sum [1..100])^2 - sum ( map (^2) [1..100] )
