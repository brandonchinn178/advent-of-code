{-
stack script --resolver lts-18.18
  --package containers
-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad ((>=>))
import Data.Char (digitToInt)
import Data.List (sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main = do
  input <- Matrix . map (map digitToInt) . lines <$> readFile "data/Day09.txt"

  let lowPoints = getLowPoints input

  -- part 1
  print $ sum $ map ((+ 1) . snd) lowPoints

  -- part 2
  let basinSizes = map (getBasinSize input . fst) lowPoints
  print $ product $ take 3 $ sortOn Down basinSizes

getLowPoints :: Matrix Int -> [(Coordinate, Int)]
getLowPoints m =
  catMaybesMatrix $
    mapMatrix
      ( \coord x ->
          if all ((> x) . snd) (matrixNeighbors m coord)
            then Just (coord, x)
            else Nothing
      )
      m

getBasinSize :: Matrix Int -> Coordinate -> Int
getBasinSize m coord0 = Set.size $ go (Set.singleton coord0) [coord0]
  where
    go seen [] = seen
    go seen (coord:rest) =
      let next =
            [ c
            | (c, x) <- matrixNeighbors m coord
            , x < 9
            , c `Set.notMember` seen
            ]
       in go (extend next seen) (next ++ rest)

    extend :: Ord a => [a] -> Set a -> Set a
    extend as = Set.union (Set.fromList as)

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

matrixNeighbors :: Matrix a -> Coordinate -> [(Coordinate, a)]
matrixNeighbors m (x, y) =
  mapMaybe
    (\coord -> (coord,) <$> matrixGet m coord)
    [ (x, y - 1) -- up
    , (x + 1, y) -- right
    , (x, y + 1) -- down
    , (x - 1, y) -- left
    ]
