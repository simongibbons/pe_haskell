import Data.Ratio

problem323 = fromRational . sum $ [1 - (1 - (1%(2^m)))^32 | m <- [0..100] ]


