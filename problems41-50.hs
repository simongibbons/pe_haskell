import Data.Char

problem42 = do
    fileIn <- readFile "data/p42.dat"
    let wordSums = map sumWord (read fileIn :: [String])
    let triNos = triangularToN $ maximum wordSums
    print $ length $ filter (\x -> x `elem` triNos) wordSums
    where
        sumWord word = sum $ map (\x -> (ord x) - 64) word
        triangularToN n = takeWhile (<n) $ map (\x -> x*(x+1) `div` 2) [1..]

problem48 = ( sum $ [x^x `mod` (10^10) | x<-[1..1000] ] ) `mod` (10^10)
