-- Find the 15th Fibonacci Golden Nugget
-- Approach: The sum of the sequence is s(x) = x / (1 - x - x^2)
--           inverting this gives x = (-(1+s) +/- sqrt(1 + 2s + 5s^2) ) / 2s
--           therefore we have only rational solutions when the expression in
--           the squareroot is a perfect square.
--
--           BruteForcing this gives the sequence (2, 15, 104, 714, 4895,
--                                                 333552, 229970, ... )
--
--           This has the pattern that the nth golden nugget is given by
--           F(2n) * F(2n+1) and thus yields the solution.

problem137 = nugget 15
  where
    --Some Mangling of the numbers to get the desired solution
    nugget n = ( fib (2*(n-1) + 1) ) * ( fib (2*(n-1) + 2) )

    fib :: Int -> Integer
    fib = (map fib' [0 ..] !!)
    fib' 0 = 1
    fib' 1 = 1
    fib' n = fib (n-2) + fib (n-1)