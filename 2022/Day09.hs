import Data.Char
import Data.Time
import qualified Data.Set as Set
import Debug.Trace

main :: IO ()
main = withTimer $ do
  input <- map parse . lines <$> readFile "2022/data/Day09.txt"

  let headPositions =
        scanl (flip ($)) (0, 0) $
          concatMap (\(dir, steps) -> replicate steps (applyDir dir)) input

  let knots = headPositions : [ getTailPositions (knots !! (i - 1)) | i <- [1..9] ]

  putStrLn $ "Part 1: " ++ show (Set.size $ Set.fromList $ knots !! 1)
  putStrLn $ "Part 2: " ++ show (Set.size $ Set.fromList $ knots !! 9)

data Dir = U | D | R | L
  deriving (Show)

parse :: String -> (Dir, Int)
parse (c : ' ' : steps) = (dir, read steps)
  where
    dir =
      case c of
        'U' -> U
        'D' -> D
        'R' -> R
        'L' -> L

applyDir :: Dir -> (Int, Int) -> (Int, Int)
applyDir dir (x, y) = (x + dx, y + dy)
  where
    (dx, dy) =
      case dir of
        U -> (0, 1)
        D -> (0, -1)
        R -> (1, 0)
        L -> (-1, 0)

getTailPositions :: [(Int, Int)] -> [(Int, Int)]
getTailPositions headPositions = tail $ scanl getNextTailPosition (0, 0) headPositions
  where
    getNextTailPosition (currX, currY) (headX, headY) =
      let dx = headX - currX
          dy = headY - currY
       in if abs dx <= 1 && abs dy <= 1
            then (currX, currY)
            else (currX + signum dx, currY + signum dy)

withTimer :: IO () -> IO ()
withTimer m = do
  start <- getCurrentTime
  m
  end <- getCurrentTime
  putStrLn $ "Elapsed: " <> show (end `diffUTCTime` start)
