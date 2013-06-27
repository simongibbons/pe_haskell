import Data.List (sort)

problem104 = snd . head . filter (\x -> isLastPan (fst x) && isFirstPan (fst x) ) $ zip fibs [1..]
    where fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
          isLastPan  n = (sort . show $ n `mod` 10^9) == "123456789"
          isFirstPan n = (sort . (take 9) $ show n ) == "123456789"

