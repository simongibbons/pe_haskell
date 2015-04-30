import Math.NumberTheory.Powers.Squares (isSquare)

problem504 = length [1 | a <- coords, b <- coords, c <- coords, d <- coords,
                         isSquare $ latPoints a b c d ]
  where coords = [1..100]
        -- Compute number of internal points using Pick's Theorem
        latPoints :: Int -> Int -> Int -> Int -> Int
        latPoints a b c d = (area2 - boundPoints) `div` 2 + 1
          where -- Twice the area of the shape
                area2 = b*c + a*b + d*c + a*d
                -- Number of points on the boundary
                boundPoints = (gcd a d) + (gcd a b) + (gcd c b) + (gcd c d)

