import Data.Char (digitToInt)

--Only have contributions with odd nos of digits
--Therefore only have to check upto 100 million.
--Brute Force runs in ~22s.
problem145 = (length.(filter isRev)) [1..100000000]
    where rev :: Int -> Int
          rev = (fromDigits.revDigs)
              where
                    fromDigits :: [Int] -> Int
                    fromDigits = foldl addDigit 0
                        where addDigit num d = 10*num + d

                    revDigs :: Int -> [Int]
                    revDigs 0 = []
                    revDigs x = (x `rem` 10):(revDigs (x `div` 10))

          allOdd :: Int -> Bool
          allOdd 0 = True
          allOdd x | x `rem` 2 == 0 = False
                   | otherwise      = allOdd (x `div` 10)

          isRev :: Int -> Bool
          isRev x | x `rem` 10 == 0 = False
                  | otherwise       = allOdd (x + (rev x) )

