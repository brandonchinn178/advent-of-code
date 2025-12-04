{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Data.Array.IArray (Array)
import Data.Array.IArray qualified as A

main :: IO ()
main = do
  input <- parse . lines <$> getContents

  printPart 1 $
    count id
      . (A.elems . (.array))
      . fmap canRemove
      . withNeighbors
      $ input

printPart :: Show a => Int -> a -> IO ()
printPart part result = putStrLn $ "Part " ++ show part ++ ": " ++ show result

data Grid a = Grid
  { array :: Array (Int, Int) a
  , curr :: (Int, Int)
  -- ^ Invariant: coordinate is within grid
  }
  deriving (Show, Eq)

instance Functor Grid where
  fmap f grid = grid{array = f <$> grid.array}

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
instance Comonad Grid where
  extract grid = grid.array A.! grid.curr
  duplicate grid = grid{array = A.genArray (A.bounds grid.array) $ \loc -> grid{curr = loc}}

data Token = Space | Roll deriving (Show, Eq)

parse :: [String] -> Grid Token
parse input =
  let grid = (map . map) toToken input
      height = length grid
      width = length $ grid !! 0
   in Grid
        { array =
            A.array ((1, 1), (width, height)) $
              [ ((x, y), val)
              | (y, row) <- zip [1..] grid
              , (x, val) <- zip [1..] row
              ]
        , curr = (0, 0)
        }
  where
    toToken = \case
      '.' -> Space
      '@' -> Roll
      c -> error $ "Unknown input: " <> show c

withNeighbors :: Grid Token -> Grid (Token, [Token])
withNeighbors = extend getNeighbors
  where
    getNeighbors grid@Grid{curr = (x, y)} =
      let neighbors =
            [ tok
            | dx <- [-1, 0, 1]
            , dy <- [-1, 0, 1]
            , not (dx == 0 && dy == 0)
            , Just tok <- pure $ grid.array A.!? (x + dx, y + dy)
            ]
       in (extract grid, neighbors)

canRemove :: (Token, [Token]) -> Bool
canRemove (tok, neighbors) = tok == Roll && count (== Roll) neighbors < 4

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f
