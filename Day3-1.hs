{-# LANGUAGE RecordWildCards #-}

module Main where

import Day3

-- | Get the layer the given number is in
getLayer :: Int -> Int
getLayer x = case evenSqrt x of
  Just sqrtInt -> widthToLayer $ if sqrtInt `mod` 2 == 0
    then sqrtInt + 1
    else sqrtInt
  Nothing -> getLayer $ x + 1

-- | Get the side the given number is in, given the layer.
getSide :: Int -> Layer -> Side
getSide x layer = if x >= bottomLeft
  then B
  else if x >= topLeft
    then L
    else if x >= topRight
      then T
      else R
  where
    Corners{..} = getCorners layer

-- | Get the distance to the middle of the side.
getDistanceToMiddle :: Int -> Layer -> Side -> Int
getDistanceToMiddle x layer side = abs $ x - middle
  where
    middle = getSideMiddle layer side

-- | Get the Hamming Distance between the given number and the
-- center of the spiral.
getDistance :: Int -> Int
getDistance x = toMid + layer
  where
    layer = getLayer x
    side = getSide x layer
    toMid = getDistanceToMiddle x layer side

main :: IO ()
main = do
  putStrLn "* Part 1:"
  check 1
  check 12
  check 23
  check 1024
  check 325489
  where
    check input = do
      putStrLn "***"
      putStrLn $ "Testing: " ++ show input
      print $ getDistance input
