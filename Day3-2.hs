{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Day3

import Data.Maybe (catMaybes)

data Turtle = Turtle
  { layer :: Layer
  , side :: Side -- changes when moving to the NEXT side (i.e. corner is the side that was before it)
  , offset :: Int -- offset from middle of side, (-layer, layer]
  } deriving (Eq, Show)

-- | The state of the next turtle.
nextTurtle :: Turtle -> Turtle
nextTurtle Turtle{..} = Turtle layer' side' offset'
  where
    side' = if offset == layer then nextSide side else side
    offset' = if offset == layer then -layer' + 1 else offset + 1
    layer' = if offset == layer && side == B then layer + 1 else layer

-- | Get all touching squares for a given Turtle.
getTouching :: Turtle -> [Int]
getTouching Turtle{..} = catMaybes [prev, minus, zero, plus]
  where
    -- the current space
    currSpace = getSpace layer side offset
    -- get the space in the previous layer from the given offset
    getSpace' = Just . getSpace (layer - 1) side . (offset +)
    -- the offset of the first square on a side
    firstOnSide = -layer + 1
    -- e.g. 15 -> 14
    prev = Just $ currSpace - 1 
    -- e.g. 15 -> 3
    minus = if offset == firstOnSide
      then if side == R
        then Nothing
        -- first on side touches square on previous side
        else Just $ currSpace - 2
      else if side == R && offset == firstOnSide + 1
        -- 'minus' in this case wraps around to last square in previous layer
        then Just $ getLayerMax $ layer - 1
        else getSpace' (-1)
    -- e.g. 15 -> 4
    zero = if offset == layer
        then if side == B
          -- last square in layer touches first square in layer
          then Just $ getLayerMin layer
          else Nothing
        else if side == R && offset == firstOnSide
          then Nothing -- handled by prev
          else getSpace' 0
    -- e.g. 15 -> 5
    plus
      | offset == layer = Nothing -- corners don't have plus
      | offset == layer - 1 && side /= B = Nothing -- plus doesn't exist yet
      | layer == 1 && side == R = Nothing
      | otherwise = getSpace' 1

-- | Get the space in the given layer with the given offset.
getSpace :: Layer -> Side -> Int -> Int
getSpace layer side offset = base + sideOffset + (layer + offset)
  where
    base = getLayerMax $ layer - 1
    widthOffset = layerToWidth layer - 1
    sideOffset = case side of
      R -> 0
      T -> widthOffset
      L -> widthOffset * 2
      B -> widthOffset * 3

-- | Get the next value in the spiral.
getNext :: (Int, Spiral, Turtle) -> (Int, Spiral, Turtle)
getNext (_, prev, t@Turtle{..}) = (nextVal, prev ++ [nextVal], nextTurtle t)
  where
    nextVal = sum $ map getVal $ getTouching t
    getVal = (prev !!) . (subtract 1) -- spaces are 1-indexed

spiral :: Spiral
spiral = map fst' $ iterate getNext (1, [1], Turtle 1 R 0)
  where
    fst' = \(x, _, _) -> x

-- | The first value written that is larger than the given input.
firstLargerThan :: Int -> Int
firstLargerThan input = head $ filter (> input) spiral

main :: IO ()
main = do
  putStrLn "* Part 2:"
  check 1
  check 12
  check 23
  check 1024
  check 325489
  where
    check input = do
      putStrLn "***"
      putStrLn $ "Testing: " ++ show input
      print $ firstLargerThan input
