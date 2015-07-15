import Data.List (sort, foldl')

-- Generates all minimax sequences using the observation that the minimax
-- sequences for a list of n items can be calculated from one with (n-1) items
-- by placing the head element in each of the middle positions.
problem336 = (!! 2010) . sort . miniMaxes $ letters
  where letters = reverse . take 11 $ ['A'..]
        miniMaxes (x:y:xs) = foldl' nextSeq [[x,y]] xs

        nextSeq :: [[a]] -> a -> [[a]]
        nextSeq [] a     = []
        nextSeq (s:xs) a = [ (take (n - k) rs) ++ [a] ++ (take k s) | k <- [1..n-1]  ] ++ nextSeq xs a
          where rs = reverse s
                n = length s


