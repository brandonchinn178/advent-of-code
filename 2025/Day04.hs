{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.Array.IArray (Array)
import Data.Array.IArray qualified as A
import Data.List (unfoldr)
import Data.Set qualified as Set

main :: IO ()
main = do
  input <- parse . lines <$> getContents

  let rollRemovals = unfoldr removeRolls input
  printPart 1 . head $ rollRemovals
  printPart 2 . sum $ rollRemovals

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
instance Applicative Grid where
  pure x = Grid (A.genArray ((1, 1), (1, 1)) (const x)) (1, 1)
  gridF <*> gridX = imapGrid (\loc f -> f $ gridX.array A.! loc) gridF

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
instance Comonad Grid where
  extract grid = grid.array A.! grid.curr
  duplicate grid = imapGrid (\loc _ -> grid{curr = loc}) grid

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

fromGrid :: Grid a -> [((Int, Int), a)]
fromGrid grid = A.assocs grid.array

imapGrid :: ((Int, Int) -> a -> b) -> Grid a -> Grid b
imapGrid f grid = grid{array = A.genArray (A.bounds grid.array) go}
  where
    go loc = f loc (grid.array A.! loc)

removeRolls :: Grid Token -> Maybe (Int, Grid Token)
removeRolls grid =
  let removed = Set.fromList . map fst . filter (canRemove . snd) . fromGrid $ withNeighbors grid
      update loc =
        if loc `Set.member` removed
          then const Space
          else id
   in if Set.null removed
        then Nothing
        else Just (length removed, imapGrid update grid)

withNeighbors :: Grid Token -> Grid (Token, [Token])
withNeighbors grid0 = (,) <$> grid0 <*> extend getNeighbors grid0
  where
    getNeighbors grid@Grid{curr = (x, y)} =
      let neighbors =
            [ tok
            | dx <- [-1, 0, 1]
            , dy <- [-1, 0, 1]
            , not (dx == 0 && dy == 0)
            , Just tok <- pure $ grid.array A.!? (x + dx, y + dy)
            ]
       in neighbors

canRemove :: (Token, [Token]) -> Bool
canRemove (tok, neighbors) = tok == Roll && count (== Roll) neighbors < 4

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f
