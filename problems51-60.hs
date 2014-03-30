import Data.Char (digitToInt, isAlpha, chr, ord)
import Data.List (sort, group, splitAt, elemIndex, maximumBy)
import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Maybe (fromJust)

problem52 = head $ [x | x<-testNos, sameDig x (2*x), sameDig x (3*x), sameDig x (4*x),
                                    sameDig x (5*x), sameDig x (6*x)]
    where getNosToTest n = takeWhile (<(17 * 10^n)) [10^(n+1)..]
          sameDig x n = (sort.show) x == (sort.show) n
          testNos = concatMap getNosToTest [1..]

problem53 = length $ [1| x<-[1..100], y<-[1..x], (choose x y) > 1000000]
    where choose n k = fac n `div` (fac (n-k) * fac k)
          fac 0 = 1
          fac x = product [1..x]


-- Problem 54
data Rank = StFlush Int
          | FourKind Int [Int]
          | FullHouse Int Int
          | Flush [Int]
          | Straight Int
          | ThreeKind Int [Int]
          | TwoPair Int Int [Int]
          | Pair Int [Int]
          | HighCard [Int]
  deriving (Show)

instance Eq Rank where
  a == b = (quality a) == (quality b)

instance Ord Rank where
  a `compare` b = (quality a) `compare` (quality b)

revSort :: Ord a => [a] -> [a]
revSort = reverse . sort

quality :: Rank -> [Int]
quality (StFlush high) = [8,high]
quality (FourKind high cards) = [7, high] ++ (revSort cards)
quality (FullHouse three pair) = [6, three, pair]
quality (Flush cards) = [5] ++ (revSort cards)
quality (Straight high) = [4, high]
quality (ThreeKind three cards) = [3, three] ++ (revSort cards)
quality (TwoPair p1 p2 cards) = [2, mxp, mip] ++ (revSort cards)
  where mxp = max p1 p2
        mip = min p1 p2
quality (Pair p cards) = [1, p] ++ (revSort cards)
quality (HighCard cards) = [0] ++ (revSort cards)


problem54 = do
              fin <- readFile "data/p54.dat"
              print $ sum $ map winner $ map parseLine $ lines $ fin
  where
    parseLine :: [Char] -> ([(Int,Char)], [(Int,Char)])
    parseLine x = splitAt 5 $ map parseCard $ words $ x

    parseCard :: [Char] -> (Int,Char)
    parseCard x = (fromJust (elemIndex (x!!0) ranks) , x!!1)
      where ranks = "123456789TJQKA"

    winner (h1, h2) | (rankHand h1) > (rankHand h2) = 1
                    | otherwise                     = 0

    rankHand :: [(Int, Char)] -> Rank
    rankHand cards | and [straight, flush] = StFlush (maximum cRanks)
                   | fourKind              = FourKind ((head.head.group) cRanks) cRanks
                   | and [threeKind, pair] = FullHouse threeKindCard pairCard
                   | flush                 = Flush cRanks
                   | straight              = Straight (maximum cRanks)
                   | threeKind             = ThreeKind threeKindCard cRanks
                   | twoPair               = TwoPair (twoPairCards!!1) (twoPairCards!!0) cRanks
                   | pair                  = Pair pairCard cRanks
                   | otherwise             = HighCard cRanks
      where cRanks = sort $ map fst cards
            gRanks = group cRanks
            cSuits = map snd cards
            flush = (length . group) cSuits == 1
            straight = cRanks == [(minimum cRanks)..(maximum cRanks)]
            fourKind = (maximum $ map length $ gRanks) == 4
            threeKind = (maximum $ map length $ gRanks) == 3
            threeKindCard = snd $ maximum $ map (\x -> (length x, head x)) $ gRanks
            twoPair = (sort $ map length gRanks) == [1,2,2]
            twoPairCards = sort $ take 2 $ map snd $
                           sort $ map (\x -> (length x, head x)) $ gRanks
            pair = (sort $ map length $ group cRanks) == [1,1,1,2]
            pairCard = snd $ maximum $ map (\x -> (length x, head x)) $ gRanks

problem55 = length $ filter (isLyr) [1..10000]
    where
        isPal x = let s = show x in s == reverse s

        revAdd x = x + revNum x

        revNum :: Integer -> Integer
        revNum x = (read.reverse.show) x

        isLyr x = isLyr' (revAdd x) 49

        isLyr' x iter
            | iter == 0 = True
            | isPal x   = False
            | otherwise = isLyr' (revAdd x) (iter -1)

problem56 = maximum $ [digitSum (a^b)| a<-[1..99], b<-[1..99] ]
    where digitSum n = sum $ map (digitToInt) (show n)

problem57 = length $ filter (numGreaterThanDenom) $ map sqrt2Convergent [1..1000]
    where numGreaterThanDenom (a,b) = (length.show) a > (length.show) b

          reduceFraction n d = let x = gcd n d
                      in case () of
                       _ | x == 1    -> (n,d)
                         | otherwise -> reduceFraction (n `div` x) (d `div` x)

          sqrt2Convergent n = let d = (denominator (n+2)) in
                              reduceFraction (numerator (n+2) - d) d

          numerator = (map numerator' [0..]!!)
          numerator' n
              | n == 0 = 0
              | n == 1 = 1
              | otherwise = 2 * (numerator (n-1) ) + (numerator (n-2) )

          denominator = (map denominator' [0..]!!)
          denominator' n
              | n == 0    = 1
              | n == 1    = 0
              | otherwise = 2 *(denominator (n-1) ) + (denominator (n-2) )

problem59 = do
    inFile <- readFile "p59.dat"
    let ctext = read inFile :: [Int]
    let c1 = [ctext!!n | n<-[0,3..(length ctext - 1)]]
    let c2 = [ctext!!n | n<-[1,4..(length ctext - 1)]]
    let c3 = [ctext!!n | n<-[2,5..(length ctext - 1)]]

    let ptext = decrypt (map findKeyNum [c1,c2,c3])  ctext
    print $ sum $ map (ord) ptext
    where
        findKeyNum c = fst $ maximumBy (comparing snd) [(k, (numLcase $ (decrypt [k] c))) | k<-[97..122] ]
        decrypt k c = map (\x -> chr ((fst x) `xor` (snd x))) $ zip (cycle k) c
        numLcase x = length $ filter isAlpha x
