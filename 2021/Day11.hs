{-
stack script --resolver lts-18.18
  --package containers
-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

import Control.Arrow ((&&&))
import Control.Monad ((>=>))
import Data.Char (digitToInt)
import Data.List (group, sort)
import Data.Maybe (mapMaybe)
import Data.Map qualified as Map
import Data.Set qualified as Set

main :: IO ()
main = do
  input <- Matrix . map (map digitToInt) . lines <$> readFile "Day11.txt"

  -- part 1
  let (_, numFlashes) = simulate 100 input
  print numFlashes

simulate :: Int -> Matrix Int -> (Matrix Int, Int)
simulate steps m = iterate runStep (m, 0) !! steps

runStep :: (Matrix Int, Int) -> (Matrix Int, Int)
runStep (m0, numFlashes) =
  let m1 = matrixMap (+ 1) m0
      (m2, flashed) = go Set.empty m1
      m3 = matrixMap (\x -> if x > 9 then 0 else x) m2
   in (m3, numFlashes + Set.size flashed)
  where
    go flashed m =
      let allFlashed = Set.fromList $ mapMaybe (\(c, x) -> if x > 9 then Just c else Nothing) (matrixElems m)
          newFlashed = allFlashed Set.\\ flashed
          flashedNeighbors = concatMap (map fst . matrixNeighbors m) . Set.toList $ newFlashed
          flashBumps = Map.fromList $ toHistogram flashedNeighbors
          m' = matrixIMap (\c x -> x + Map.findWithDefault 0 c flashBumps) m
       in if Set.null newFlashed then (m, flashed) else go allFlashed m'

{-- Matrix --}

newtype Matrix a = Matrix {unMatrix :: [[a]]}
  deriving (Show)

matrixShow :: Show a => Matrix a -> String
matrixShow = unlines . map (concatMap show) . unMatrix

type Coordinate = (Int, Int)

matrixGet :: Matrix a -> Coordinate -> Maybe a
matrixGet m (x, y) = (getIndex y >=> getIndex x) . unMatrix $ m
  where
    getIndex i xs = if 0 <= i && i < length xs then Just (xs !! i) else Nothing

matrixMap :: (a -> b) -> Matrix a -> Matrix b
matrixMap f = Matrix . map (map f) . unMatrix

matrixIMap :: (Coordinate -> a -> b) -> Matrix a -> Matrix b
matrixIMap f = Matrix . go . unMatrix
  where
    go = imap $ \y -> imap $ \x -> f (x, y)

    imap :: (Int -> a -> b) -> [a] -> [b]
    imap f = zipWith f [0 ..]

matrixElems :: Matrix a -> [(Coordinate, a)]
matrixElems = concat . unMatrix . matrixIMap (,)

matrixNeighbors :: Matrix a -> Coordinate -> [(Coordinate, a)]
matrixNeighbors m (x, y) =
  mapMaybe
    (\coord -> (coord,) <$> matrixGet m coord)
    [ (x + dx, y + dy)
    | dx <- [-1 .. 1]
    , dy <- [-1 .. 1]
    , (dx, dy) /= (0, 0)
    ]

{-- Utilities --}

toHistogram :: Ord a => [a] -> [(a, Int)]
toHistogram = map (head &&& length) . group . sort
