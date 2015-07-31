import Math.NumberTheory.Moduli (powerMod)

-- (a, b, c) are the roots of a third degree polynomial
--  (a + b)*(b + c)*(c + a) = (a + b + c)*(ab + bc + ca) - abc
--
-- Thus can be expressed in terms of the coefficients of the polynomial
-- yielding
--   (a + b)*(b + c)*(c + a) = 1 - k^2
--
-- The sum over p is then a geometric progression giving
--  S(n) = \sum_{k=1}^n (k^2 - 1) * ( (1-k^2)^n - 1) / k^2
-- The sum over n is computed in a loop, noting that
-- the modulus given is prime, thus we can use an efficient powerMod
-- operation and Fermat's Little Theorem to simplify the (1/k^2) (mod p) term.
problem479 = (sum . map sk $ [1..n]) `mod` p
  where
    n = 10^6
    p = 1000000007

    sk k = (f1 * f2 * ( f3 - 1) ) `mod` p
      where f1 = (k^2 - 1) `mod` p
            f2 = powerMod k (2*p - 4) p
            f3 = powerMod (1-k^2) n p

