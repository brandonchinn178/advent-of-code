{-
stack script --resolver lts-18.18
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.List (sort)
import Data.Maybe (listToMaybe, mapMaybe)

main :: IO ()
main = do
  input <- map (map parseToken) . lines <$> readFile "data/Day10.txt"

  -- part 1
  let getCorruptedScore = \case
        Corrupted{_actual = Token{tokenType}} -> Just (getCorruptedPoints tokenType)
        _ -> Nothing
      getCorruptedPoints = \case
        PAREN -> 3
        BRACK -> 57
        BRACE -> 1197
        ANGLE -> 25137
  print $ sum $ mapMaybe (getCorruptedScore . checkLine) input

  -- part 2
  let getMissingScore = \case
        Incomplete{_missingTokens = tokens} ->
          Just $
            foldl (\acc x -> acc * 5 + x) 0 $
              map (getMissingPoints . tokenType) tokens
        _ -> Nothing
      getMissingPoints = \case
        PAREN -> 1
        BRACK -> 2
        BRACE -> 3
        ANGLE -> 4
      getMedian xs = sort xs !! (length xs `div` 2)
  print $ getMedian $ mapMaybe (getMissingScore . checkLine) input

data Token = Token
  { tokenType :: TokenType
  , isOpen :: Bool
  }
  deriving (Show, Eq)

data TokenType
  = PAREN
  | BRACK
  | BRACE
  | ANGLE
  deriving (Show, Eq)

parseToken :: Char -> Token
parseToken = \case
  '(' -> Token PAREN True
  ')' -> Token PAREN False
  '[' -> Token BRACK True
  ']' -> Token BRACK False
  '{' -> Token BRACE True
  '}' -> Token BRACE False
  '<' -> Token ANGLE True
  '>' -> Token ANGLE False
  c -> error $ "Invalid token: " ++ show c

flipToken :: Token -> Token
flipToken token = token{isOpen = not $ isOpen token}

data CheckResult
  = Valid
  | Incomplete
      { _missingTokens :: [Token]
      }
  | Corrupted
      { _expected :: Maybe Token
      , _actual :: Token
      }
  deriving (Show)

checkLine :: [Token] -> CheckResult
checkLine = go []
  where
    go = curry $ \case
      ([], []) -> Valid
      (expected, []) -> Incomplete{_missingTokens = expected}
      (expected, t : ts) | isOpen t -> go (flipToken t : expected) ts
      (t' : expected, t : ts) | t == t' -> go expected ts
      (expected, t : _) ->
        Corrupted
          { _expected = listToMaybe expected
          , _actual = t
          }
