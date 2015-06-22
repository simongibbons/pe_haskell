import Data.Ratio


-- For any colour the probability that it will be present is
-- P(Present) = 1 - P(Absent)
--            = 1 - (no of ways to pick 20 so that a colour is absent) /
--                  (no of ways to pick 20)
problem493 = fromRational $ 7 * (1 - (60 `choose` 20) % (70 `choose` 20))
  where n `choose` k = (product [n + 1 - i | i <- [1..k]]) `div` (product [1..k])
