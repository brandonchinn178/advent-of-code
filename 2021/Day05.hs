{-
stack script --resolver lts-18.18
  --package text
-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Arrow ((&&&))
import Data.List (group, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Text.Read (readMaybe)

main :: IO ()
main = do
  input <- map parseLine . Text.lines <$> Text.readFile "Day05.txt"

  let countOverlaps = length . filter ((>= 2) . snd) . toHistogram

  -- part 1
  print $ countOverlaps $ concat $ mapMaybe toPoints $ filter isStraight input

  -- part 2
  print $ countOverlaps $ concat $ mapMaybe toPoints input

type Point = (Int, Int)
type Line = (Point, Point)

parseLine :: Text -> Line
parseLine line =
  fromMaybe (error $ "Invalid line: " ++ show line) $ do
    (p1, p2) <- splitOn2 " -> " line
    (,) <$> parsePoint p1 <*> parsePoint p2
  where
    parsePoint p = do
      (x, y) <- splitOn2 "," p
      (,) <$> readInt x <*> readInt y

    readInt = readMaybe @Int . Text.unpack

    splitOn2 sep t =
      case Text.splitOn sep t of
        [a, b] -> Just (a, b)
        _ -> Nothing

isStraight :: Line -> Bool
isStraight ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

toPoints :: Line -> Maybe [Point]
toPoints ((x1, y1), (x2, y2))
  | dx == 0 = Just [(x1, y1 + signY * d) | d <- [0 .. dy]]
  | dy == 0 = Just [(x1 + signX * d, y1) | d <- [0 .. dx]]
  | dx == dy = Just [(x1 + signX * d, y1 + signY * d) | d <- [0 .. dx]]
  | otherwise = Nothing
  where
    (dx, signX) = (abs &&& signum) (x2 - x1)
    (dy, signY) = (abs &&& signum) (y2 - y1)

toHistogram :: Ord a => [a] -> [(a, Int)]
toHistogram = map collect . group . sort
  where
    collect xs@(x:_) = (x, length xs)
