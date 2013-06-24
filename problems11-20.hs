import Data.Char

problem16 = power2sum 1000
    where power2sum n = sum $ map digitToInt $ show (2^n)

problem20 = facSum 100
    where facSum n = sum $ map digitToInt $ show (fac n)
          fac n    = product [1..n]
