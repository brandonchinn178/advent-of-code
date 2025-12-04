{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List (unfoldr)
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main = do
  input <- parse . lines <$> getContents

  let rollRemovals = unfoldr removeRolls input
  printPart 1 . head $ rollRemovals
  printPart 2 . sum $ rollRemovals

printPart :: Show a => Int -> a -> IO ()
printPart part result = putStrLn $ "Part " ++ show part ++ ": " ++ show result

type Coordinate = (Int, Int)

parse :: [[Char]] -> Set Coordinate
parse input =
  Set.fromList
    [ (x, y)
    | (y, row) <- zip [0..] input
    , (x, val) <- zip [0..] row
    , val == '@'
    ]

canRemove :: Set Coordinate -> Coordinate -> Bool
canRemove coords (x, y) = length neighbors < 4
  where
    neighbors =
      [ coord
      | dx <- [-1, 0, 1]
      , dy <- [-1, 0, 1]
      , let coord = (x + dx, y + dy)
      , not (dx == 0 && dy == 0)
      , coord `Set.member` coords
      ]

removeRolls :: Set Coordinate -> Maybe (Int, Set Coordinate)
removeRolls coords =
  let (removed, coords') = Set.partition (canRemove coords) coords
   in if Set.null removed
        then Nothing
        else Just (length removed, coords')
