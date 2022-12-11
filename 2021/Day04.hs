{-
stack script --resolver lts-18.18
  --package text
-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Data.Foldable (foldlM)
import Data.List (maximumBy, minimumBy, transpose)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

main :: IO ()
main = do
  (nums, boards) <- parseInput <$> Text.readFile "data/Day04.txt"

  -- part 1
  let fastestRound = getWinningRoundInfoBy (Down . roundNum) nums boards
  print $ scoreRound fastestRound

  -- part 2
  let slowestRound = getWinningRoundInfoBy roundNum nums boards
  print $ scoreRound slowestRound

parseInput :: Text -> ([Int], [BoardVals])
parseInput t =
  case Text.lines t of
    [] -> invalid
    nums : rest -> (parseNums nums, map Board $ parseBoards rest)
  where
    invalid = error $ "Invalid input: " ++ Text.unpack t

    parseNums = map (read @Int . Text.unpack) . Text.splitOn ","
    parseBoards = \case
      [] -> []
      "" : r1 : r2 : r3 : r4 : r5 : rest -> map parseRow [r1, r2, r3, r4, r5] : parseBoards rest
      _ -> invalid
    parseRow = map (read @Int . Text.unpack) . Text.words

-- | 5x5 board
newtype BoardOf a = Board {unBoard :: [[a]]}
type BoardVals = BoardOf Int
type Board = BoardOf (Int, Bool)

instance Functor BoardOf where
  fmap f = Board . map (map f) . unBoard

isWinner :: Board -> Bool
isWinner (Board rows) = any (all snd) rows || any (all snd) (transpose rows)

getAllUnmarked :: Board -> [Int]
getAllUnmarked = concatMap (map fst . filter (not . snd)) . unBoard

data RoundInfo = RoundInfo
  { board :: Board
  , roundNum :: Int
  , calledNum :: Int
  }

scoreRound :: RoundInfo -> Int
scoreRound RoundInfo{..} = calledNum * sum (getAllUnmarked board)

getWinningRoundInfoBy :: Ord a => (RoundInfo -> a) -> [Int] -> [BoardVals] -> RoundInfo
getWinningRoundInfoBy f nums boards =
  case mapMaybe (roundsToWin nums) boards of
    [] -> error "No boards won"
    allWonRoundsInfo -> maximumBy (comparing f) allWonRoundsInfo

-- For the given board and list of called numbers, return the
-- round info of the round when the Board won.
roundsToWin :: [Int] -> BoardVals -> Maybe RoundInfo
roundsToWin nums boardVals =
  either Just (const Nothing) $
    foldlM go initialRoundInfo calledNumAndRoundNums
  where
    initialRoundInfo =
      RoundInfo
        { board = fmap (, False) boardVals
        , roundNum = 0
        , calledNum = 0
        }

    -- called numbers annotated with the round number
    calledNumAndRoundNums = zip nums [1..]

    go :: RoundInfo -> (Int, Int) -> Either RoundInfo RoundInfo
    go RoundInfo{board} (calledNum, roundNum) =
      let newBoard = setNum calledNum <$> board
          roundInfo = RoundInfo{board = newBoard, ..}
       in if isWinner newBoard
            then Left roundInfo -- short-circuit when board wins
            else Right roundInfo

    setNum :: Int -> (Int, Bool) -> (Int, Bool)
    setNum num spot@(x, _) = if num == x then (x, True) else spot
