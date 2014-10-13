import Data.List (sort)
import Data.Ratio

type Row = [ Ratio Int ]
type Matrix = [ Row ]

problem101 = sum $ map firstIncorrectTerm [1..10]
  where
    firstIncorrectTerm k = fst.head $ filter (\(a,b) -> a /= b ) $ zip opTerms trueTerms
      where
        opTerms  = map (evalPoly opCoeffs) [1..]
        opCoeffs = optimalCoeffs (take k $ map numerator trueTerms)

    optimalCoeffs u = solve m
      where m = [([(i^n % 1) | n<-[0..(l-1)] ] ++ [(u!!(i-1)) % 1]) | i<-[1..l] ]
            l = length u 

    evalPoly :: Num a => [a] -> a -> a
    evalPoly l n = sum $ map (\(c,o) -> c * n^o) $ zip l [0..]

    trueCoeffs = take 11 [ (-1)^n | n<-[0..] ]

    trueTerms :: [Ratio Int]
    trueTerms  = map (evalPoly trueCoeffs) [1..]

    gaussianReduce :: Matrix -> Matrix
    gaussianReduce matrix = fixlastrow $ foldl reduceRow matrix [0..length matrix-1] where
     --swaps element at position a with element at position b.
     swap xs a b
      | a > b = swap xs b a
      | a == b = xs
      | a < b = let
      (p1,p2) = splitAt a xs
      (p3,p4) = splitAt (b-a-1) (tail p2)
      in p1 ++ [xs!!b] ++ p3 ++ [xs!!a] ++ (tail p4)

     reduceRow matrix1 r = let
      --first non-zero element on or below (r,r).
      firstnonzero = head $ filter (\x -> matrix1 !! x !! r /= 0) [r..length matrix1-1]

      --matrix with row swapped (if needed)
      matrix2 = swap matrix1 r firstnonzero

      --row we're working with
      row = matrix2 !! r

      --make it have 1 as the leading coefficient
      row1 = map (\x -> x / (row !! r)) row

      --subtract nr from row1 while multiplying
      subrow nr = let k = nr!!r in zipWith (\a b -> k*a - b) row1 nr

      --apply subrow to all rows below
      nextrows = map subrow $ drop (r+1) matrix2

      --concat the lists and repeat
      in take r matrix2 ++ [row1] ++ nextrows

     fixlastrow matrix' = let
      a = init matrix'; row = last matrix'; z = last row; nz = last (init row)
      in a ++ [init (init row) ++ [1, z / nz]]

    substitute :: Matrix -> Row
    substitute matrix = foldr next [last (last matrix)] (init matrix) where

     next row found = let
      subpart = init $ drop (length matrix - length found) row
      solution = last row - sum (zipWith (*) found subpart)
      in solution : found

    solve :: Matrix -> Row
    solve = substitute . gaussianReduce

problem102 = do
              fin <- readFile "data/p102.dat"
              let points = map (listToPoints.lineToList) $ lines fin
              print $ length $ filter containsOrigin points
  where
    triangleArea :: (Int,Int) -> (Int, Int) -> (Int,Int) -> Int
    triangleArea (x1, y1) (x2, y2) (x3, y3) = abs $
                                              (x1 - x3)*(y2 - y1) - (x1 - x2)*(y3 - y1)

    containsOrigin :: [(Int,Int)] -> Bool
    containsOrigin (p1:p2:p3:[]) = triangleArea p1 p2 p3 == (triangleArea p1 p2 (0,0) +
                                                             triangleArea p1 (0,0) p3 +
                                                             triangleArea (0,0) p2 p3 )

    lineToList :: String -> [Int]
    lineToList s = read $ "[" ++ s ++ "]"

    listToPoints :: [Int] -> [(Int,Int)]
    listToPoints [] = []
    listToPoints (x:y:xs) = (x,y) : listToPoints xs

problem104 = snd . head . filter (\x -> isLastPan (fst x) && isFirstPan (fst x) ) $ zip fibs [1..]
    where fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
          isLastPan  n = (sort . show $ n `mod` 10^9) == "123456789"
          isFirstPan n = (sort . (take 9) $ show n ) == "123456789"

-- The number of solutions is (numDivisors (n^2)) / 2 + 1
problem108 = fst . head $
             dropWhile (\(_,ns) -> ns < 1000) $
             map (\x -> (x, numSolutions x)) [1..]
  where numSolutions n =  ((divCountSq n) `div` 2) + 1
        divCountSq n = product $ map (\(_,a) -> 2*a + 1) $ factorise n
