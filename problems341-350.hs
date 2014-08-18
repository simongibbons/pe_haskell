import qualified Data.Array.Unboxed as A
import Data.Algorithm.Munkres

problem345 = do
    fin <- readFile "data/p345.dat"
    let matrix = (read fin) :: [Int]

    print $ matrixSum matrix
  where matrixSum :: [Int] -> Int
        matrixSum matrix = sum $ map (mArray A.!) elements
          where modifiedMatrix = A.listArray ((1,1),(15,15)) $ map (\x -> m - x) matrix
                    where m = maximum matrix

                mArray :: A.UArray (Int, Int) Int
                mArray = A.listArray ((1,1),(15,15)) matrix

                (elements,_) = hungarianMethodInt modifiedMatrix


