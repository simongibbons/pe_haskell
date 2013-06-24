problem63 = sum $ takeWhile (\x -> numNdigitNthPower x > 0) [numNdigitNthPower x | x<-[1..]]
    where
        getPowerNos n = takeWhile (\x -> (length $ show (x^n)) <= n) [1..]
        numNdigitNthPower n = length $ filter (\x -> ((length $ show (x^n)) == n) ) $ getPowerNos n

problem67 = do
    inFile <- readFile "data/p67.dat"
    print $ maxPathSum (read inFile :: [[Int]])
    where
        maxPathSum = head . foldr1 step
        step [] [z] = [z]
        step (x:xs) (y:z:zs) = x + max y z : step xs (z:zs)

