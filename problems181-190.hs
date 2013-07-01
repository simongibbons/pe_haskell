problem188 = tetrate (10^8) 1777 1855
    where powerMod m b e = powm b e m 1 

          powm :: Integer -> Integer -> Integer -> Integer -> Integer
          powm b 0 m r = r
          powm b e m r | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
          powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

          tetrate m a 1 = a
          tetrate m a k = powerMod m a (tetrate m a (k-1))
