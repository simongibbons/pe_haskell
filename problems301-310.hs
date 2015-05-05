import Data.Bits (xor)

problem301 = length [() | n <- [1..2^(30)] :: [Int] , (n `xor` (2*n) `xor` (3*n)) == 0]

