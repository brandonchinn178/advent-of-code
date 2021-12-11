{-
stack script --resolver lts-18.18
-}

import Data.Char (digitToInt)
import Data.List (transpose)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
  input <- map (map digitToInt) . lines <$> readFile "Day09.txt"

  -- part 1
  print $ sum . map (+ 1) . getLowPoints $ input

getLowPoints :: [[Int]] -> [Int]
getLowPoints xs =
  catMaybes . concat $
    zipMatrices
      (\x isLow -> if isLow then Just x else Nothing)
      xs
      lowPoints
  where
    lowPoints =
      zipMatrices
        (&&)
        (map getLowPointsForRow $ xs)
        (transpose . map getLowPointsForRow . transpose $ xs)

    zipMatrices :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
    zipMatrices f = zipWith (zipWith f)

getLowPointsForRow :: [Int] -> [Bool]
getLowPointsForRow xs =
  zipWith3
    (\pre x post -> x < pre && x < post)
    (inf : xs)
    xs
    (tail xs ++ [inf])
  where
    inf = 1000 -- all numbers in row are single digits
