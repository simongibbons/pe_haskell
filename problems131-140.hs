import Data.List (sort, group)
import Math.NumberTheory.Primes.Testing (isPrime)

{- n^3 + p*n^2 can only be a cube if n and p + n are cubes
 - Therefore p must be the difference of two cubes.
 - Only the difference of two consecutive cubes can be prime.
 -}
problem131 = length $ filter isPrime $ takeWhile (<10^6) $
             map (\x -> (x+1)^3 - x^3) [1..]

problem135 = length $ filter (==10) $ map length $ group $ sort sols
  where
    limit = 1000000
    sols = [ u*v | u <- [1..limit],
                   v<-[1..(limit `div` u)],
                   (u + v) `mod` 4 == 0,
                   3*v > u,
                   (3*v - u) `mod` 4  == 0 ]

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

-- Algebra gives that for  4 + 5*(L^2 - 1) to be a perfect square
-- As above I brute force the first few terms and notice a pattern
-- to obtain the general form of the solution.
problem138 = sum $ map (\x -> fib (6*x + 2) `div` 2) $ [1..12]
  where
    fib :: Int -> Integer
    fib = (map fib' [0 ..] !!)
    fib' 0 = 1
    fib' 1 = 1
    fib' n = fib (n-2) + fib (n-1)

-- Maths magic turns this into Pell's equation.
problem139 = sum $ map (\(x,y) -> limit `div` (x+y)) $ drop 1 $
                   takeWhile (\(x,y) -> (x+y) < limit) $ iterate newSol (1,1)
  where limit = 100000000
        newSol (x,y) = ( 3*x + 4*y, 2*x + 3*y )
