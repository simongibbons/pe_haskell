import Data.List (foldl')

problem401 = (sumSingle + sumGroup) `mod` m
  where
    n = 10^15
    m = 10^9

    single = floor . sqrt . fromIntegral $ n

    kend = n `div` single

    sum' :: Num a => [a] -> a
    sum' = foldl' (+) 0

    sumSingle = (`mod` m) . sum' . map term $ [1..single]
      where term i = ((n `div` i) * i^2) `mod` m

    sumSqMod lo hi m = ((t1 * t2) `div` 6) `mod` m
      where t1 = 1 + hi - lo
            t2 = (hi + 2*hi^2 - lo + 2*hi*lo + 2*lo^2)


    sumGroup = sum' . map term $ [1..(kend - 1)]
      where term k = k * (sumSqMod lo hi m)
              where lo = (n `div` (1+k)) + 1
                    hi = n `div` k
