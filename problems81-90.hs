import qualified Data.Map as M

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
