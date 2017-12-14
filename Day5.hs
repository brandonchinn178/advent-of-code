{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.HashMap.Lazy (HashMap, empty, lookupDefault, insert)

import Utils

getPuzzle :: IO [Int]
getPuzzle = (map read . lines) <$> readFile "day5.txt"

type Deltas = HashMap Int Int

data State = State
  { deltas :: Deltas
  , index :: Int
  } deriving (Eq, Show)

-- | Get the next state given the current state.
nextState :: (Int -> Int) -> [Int] -> State -> State
nextState incFunc list State{..} = State nextDeltas $ index + val
  where
    nextDeltas = insert index (incFunc val) deltas
    val = list !! index + delta
    delta = lookupDefault 0 index deltas

totalSteps :: (Int -> Int) -> [Int] -> Int
totalSteps incFunc list = go $ State empty 0
  where
    go s = if index s < length list
      then 1 + (go $ nextState incFunc list s)
      else 0

part1IncFunc :: Int -> Int
part1IncFunc = (1 +)

part2IncFunc :: Int -> Int
part2IncFunc x = if x >= 3 then x - 1 else x + 1

main :: IO ()
main = do
  part 1 $ do
    check "totalSteps2" totalSteps1
      [ ([0, 3, 0, 1, -3], 5)
      ]
    getPuzzle >>= solve . totalSteps1

  part 2 $ do
    check "totalSteps2" totalSteps2
      [ ([0, 3, 0, 1, -3], 10)
      ]
    getPuzzle >>= solve . totalSteps2
  where
    totalSteps1 = totalSteps part1IncFunc
    totalSteps2 = totalSteps part2IncFunc
