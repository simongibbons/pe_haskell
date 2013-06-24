import Data.Char

problem15 = choose 40 20
    where choose n k = product [(k+1)..n] `div` product [1..k]

problem16 = power2sum 1000
    where power2sum n = sum $ map digitToInt $ show (2^n)

problem20 = facSum 100
    where facSum n = sum $ map digitToInt $ show (fac n)
          fac n    = product [1..n]
