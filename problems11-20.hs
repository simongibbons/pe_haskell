import Data.Char

problem16 = power2sum 1000
    where power2sum n = sum (map digitToInt (show (2^n)))
