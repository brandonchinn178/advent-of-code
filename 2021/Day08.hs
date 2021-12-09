{-
stack script --resolver lts-18.18
  --package containers
  --package text
-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text

main :: IO ()
main = do
  input <- map parsePuzzle . lines <$> readFile "Day08.txt"

  -- part 1
  let uniqueNumWires = [2, 4, 3, 7] -- # of wires for 1, 4, 7, and 8
  print $ length $ filter ((`elem` uniqueNumWires) . length) $ concatMap outputPatterns input

data Puzzle = Puzzle
  { inputPatterns :: [SignalPattern]
  , outputPatterns :: [SignalPattern]
  }
  deriving (Show)

type SignalPattern = Set SignalWire

data SignalWire
  = WireA
  | WireB
  | WireC
  | WireD
  | WireE
  | WireF
  | WireG
  deriving (Show, Eq, Ord)

parsePuzzle :: String -> Puzzle
parsePuzzle s =
  fromMaybe (error $ "Invalid puzzle: " ++ show s) $ do
    [inputPatterns, outputPatterns] <- pure $ splitOn " | " s
    Puzzle <$> parsePatterns inputPatterns <*> parsePatterns outputPatterns
  where
    parsePatterns = mapM (fmap Set.fromList . mapM parseWire) . words
    parseWire = \case
      'a' -> Just WireA
      'b' -> Just WireB
      'c' -> Just WireC
      'd' -> Just WireD
      'e' -> Just WireE
      'f' -> Just WireF
      'g' -> Just WireG
      _ -> Nothing

data Digit
  = Digit0
  | Digit1
  | Digit2
  | Digit3
  | Digit4
  | Digit5
  | Digit6
  | Digit7
  | Digit8
  | Digit9
  deriving (Show)

digitToPattern :: Digit -> SignalPattern
digitToPattern =
  Set.fromList . \case
    Digit0 -> [WireA, WireB, WireC, WireE, WireF, WireG]
    Digit1 -> [WireC, WireF]
    Digit2 -> [WireA, WireC, WireD, WireE, WireG]
    Digit3 -> [WireA, WireC, WireD, WireF, WireG]
    Digit4 -> [WireB, WireC, WireD, WireF]
    Digit5 -> [WireA, WireB, WireD, WireF, WireG]
    Digit6 -> [WireA, WireB, WireD, WireE, WireF, WireG]
    Digit7 -> [WireA, WireC, WireF]
    Digit8 -> [WireA, WireB, WireC, WireD, WireE, WireF, WireG]
    Digit9 -> [WireA, WireB, WireC, WireD, WireF, WireG]

{-- Utilities --}

splitOn :: String -> String -> [String]
splitOn sep = map Text.unpack . Text.splitOn (Text.pack sep) . Text.pack
