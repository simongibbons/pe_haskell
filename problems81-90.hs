import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Ord (comparing)
import Data.List (minimumBy, findIndex)
import Data.Maybe (fromJust)
import Math.NumberTheory.Powers.Squares (isSquare)

primesToLimit :: Integer -> [Integer]
primesToLimit m = 2 : sieve [3,5..m]
  where
    sieve (p:xs)
      | p*p > m = p : xs
      | True    = p : sieve [x | x <- xs, rem x p /= 0]

problem81 = readFile "data/p81.txt" >>= print . minSum . map parse . lines
  where
    parse :: String -> [Int]
    parse = read . ('[':) . (++ "]")

    minSum :: [[Int]] -> Int
    minSum (x:xs) = last $ (foldl nextLine) (scanl1 (+) x) xs

    nextLine :: [Int] -> [Int] -> [Int]
    nextLine (p:pl) (n:nl) = scanl nextCell (p+n) (zip pl nl)
        where nextCell acc (prev, new) = new + min prev acc

-- Find the Area of the Rectangle with the number of rectangles closet
-- to 2 million
problem85 = minimumBy (comparing snd)
            [(n*m, abs ( (numRecs n m) - 2*10^6))| n<-[1..100], m<-[1..n]  ]
  where
    tri n = n*(n+1) `div` 2 -- (n-1) choose 2
    numRecs n m = (tri n) * (tri m)

problem86 = fromJust . findIndex (>10^6) . scanl (+) 0 . map cube $ [1..]
  where cube m = sum [(a `div` 2) - x | a <- [1..2*m], isSquare (a^2 + m^2),
                      let x = if a > m then (a - m - 1) else 0]

-- Find the number of numbers expressible as the sum of a prime square, cube
-- and fourth power below 50 million.
problem87 = Set.size $ Set.fromList $
            [a + b + c| a<-prime4,
                        b<-(takeWhile (<limit-a) prime3),
                        c<-(takeWhile (<=limit-a-b) prime2) ]
  where
    limit = 50000000
    primes = primesToLimit 7071
    prime2 = takeWhile (< limit) $ map (^2) primes
    prime3 = takeWhile (< limit) $ map (^3) primes
    prime4 = takeWhile (< limit) $ map (^4) primes

problem89 = do
        contents <- readFile "data/p89.dat"
        let numerals = lines contents
        print $ sum $ map numCharsSaved numerals
        where
            numCharsSaved x = length x - (length.makeRom.parseRom) x

            --It would be quicker to RegEx this and replace "IIII" -> "IV" etc.
            makeRom x
                | x == 0               = ""
                | x <  4               = 'I':(makeRom  (x-1))
                | x == 4               = "IV"
                | x <  9               = 'V':(makeRom  (x-5))
                | x == 9               = "IX"
                | x < 40               = 'X':(makeRom  (x-10))
                | 40 <= x && x < 50    = "XL" ++ (makeRom (x-40))
                | x < 90               = 'L':(makeRom  (x-50))
                | 90 <= x && x < 100   = "XC" ++ (makeRom (x-90))
                | x < 400              = 'C':(makeRom (x-100))
                | 400 <= x && x < 500  = "CD" ++ (makeRom (x-400))
                | x < 900              = 'D':(makeRom (x-500))
                | 900 <= x && x < 1000 = "CM" ++ (makeRom (x-900))
                | otherwise            = 'M':(makeRom (x-1000))

            parseRom x = parseRom' $ map romToInt x

            parseRom' (x:xs)
                | xs == []      = x
                | x < (head xs) = (-x) + parseRom' xs
                | otherwise     = x + parseRom' xs

            romMap = M.fromList $ zip ['I','V','X','L','C','D','M'] [1,5,10,50,100,500,1000]

            romToInt x = romMap M.! x

