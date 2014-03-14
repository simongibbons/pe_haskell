import Data.List (sort)

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

