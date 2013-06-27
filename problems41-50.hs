import Data.Char

problem42 = do
    fileIn <- readFile "data/p42.dat"
    let wordSums = map sumWord (read fileIn :: [String])
    let triNos = triangularToN $ maximum wordSums
    print $ length $ filter (\x -> x `elem` triNos) wordSums
    where
        sumWord word = sum $ map (\x -> (ord x) - 64) word
        triangularToN n = takeWhile (<n) $ map (\x -> x*(x+1) `div` 2) [1..]

problem44 = head [x-y | x<-pents, y<-takeWhile (<x) pents, isPent (x+y), isPent (x-y)]
    where isPent n =
            let (ai, af) = properFraction . sqrt $ 1 + 24 * (fromInteger n)
            in  (af == 0) && ai `mod` 6 == 5

          pents = [n*(3*n-1) `div` 2 | n<-[1..2500] ]

problem_45 = head [x | x <- scanl (+) 1 [5,9..], x > 40755, isPent x]
    where isPent n =
            let (ai, af) = properFraction . sqrt $ 1 + 24 * (fromInteger n)
            in  (af == 0) && ai `mod` 6 == 5

problem48 = ( sum $ [x^x `mod` (10^10) | x<-[1..1000] ] ) `mod` (10^10)
