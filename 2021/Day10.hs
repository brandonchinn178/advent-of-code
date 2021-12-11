{-
stack script --resolver lts-18.18
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.Maybe (listToMaybe, mapMaybe)

main :: IO ()
main = do
  input <- map (map parseToken) . lines <$> readFile "Day10.txt"

  -- part 1
  let getCorruptedScore = \case
        Corrupted{_actual = Token{tokenType}} -> Just (getPoints tokenType)
        _ -> Nothing
      getPoints = \case
        PAREN -> 3
        BRACK -> 57
        BRACE -> 1197
        ANGLE -> 25137
  print $ sum $ mapMaybe (getCorruptedScore . checkLine) input

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
      (_, []) -> Incomplete
      (hist, t : ts) | isOpen t -> go (t : hist) ts
      (t1 : hist, t2 : ts) | flipToken t1 == t2 -> go hist ts
      (hist, t : _) ->
        Corrupted
          { _expected = flipToken <$> listToMaybe hist
          , _actual = t
          }
