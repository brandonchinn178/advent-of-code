{-
stack script --resolver lts-18.18
-}

import Control.Monad ((>=>))
import Data.Char (digitToInt)
import Data.Maybe (catMaybes, mapMaybe)

main :: IO ()
main = do
  input <- Matrix . map (map digitToInt) . lines <$> readFile "Day09.txt"

  -- part 1
  print $ sum . map (+ 1) . getLowPoints $ input

getLowPoints :: Matrix Int -> [Int]
getLowPoints m =
  catMaybesMatrix $
    mapMatrix
      ( \coord x ->
          if all (> x) $ mapMaybe (matrixGet m) (neighbors coord)
            then Just x
            else Nothing
      )
      m

{-- Matrix --}

newtype Matrix a = Matrix {unMatrix :: [[a]]}

type Coordinate = (Int, Int)

mapMatrix :: (Coordinate -> a -> b) -> Matrix a -> Matrix b
mapMatrix f = Matrix . withIndex (\y -> withIndex (\x -> f (x, y))) . unMatrix
  where
    withIndex f = zipWith f [0 ..]

catMaybesMatrix :: Matrix (Maybe a) -> [a]
catMaybesMatrix = catMaybes . concat . unMatrix

matrixGet :: Matrix a -> Coordinate -> Maybe a
matrixGet m (x, y) = (arrayGet y >=> arrayGet x) . unMatrix $ m
  where
    arrayGet i xs = if 0 <= i && i < length xs then Just (xs !! i) else Nothing

neighbors :: Coordinate -> [Coordinate]
neighbors (x, y) =
  [ (x, y - 1) -- up
  , (x + 1, y) -- right
  , (x, y + 1) -- down
  , (x - 1, y) -- left
  ]
