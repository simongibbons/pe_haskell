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

-- This is very slow and uses a hell of a lot of memory -- should investigate
-- using mutable arrays to do the counting as we go along.
problem136 = length $ filter (==1) $ map length $ group $ sort $ solutions
  where solutions = [u*v | u <- [1..limit],
                           v <- [1..(limit `div` u)],
                           3*u > v,
                           (u + v) `mod` 4 == 0,
                           (3*u - v) `mod` 4 == 0 ]
        limit = 50000000

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

-- 1) Derive generating function for the series as
--     Ag = (x + 3x^2 ) / (1 - x - x^2 )
-- 2) This takes integer values Ag = n for rational x
--    when 5n^2 + 14n + 1 is a perfect square.
-- 3) This can be written in the form of a Pell equation with
--    a^2 - 5p^2 == 44
--    with n = (a - 7) / 5
--    This solutions for a are found by the recurrance
--    a i = 3 * a (i-2) - a (i-4)
-- 4) When (a - 7) / 5 is an integer, this forms a golden nugget.
problem140 = sum $ take 30 $
             map (\n -> (n-7) `div` 5 ) $
             filter (\n -> (n-7) `mod` 5 == 0) $
             map a [1..]
  where
    a n = (map a' [0..]) !! n
    a' 0 = 7
    a' 1 = 8
    a' 2 = 13
    a' 3 = 17
    a' n = 3 * a (n-2) - a (n-4)
