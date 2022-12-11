{-
stack script --resolver lts-18.18
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

data Command
  = Forward Int
  | Up Int
  | Down Int
  deriving (Show)

parseCommand :: String -> Command
parseCommand s =
  case words s of
    ["forward", x] -> Forward (read x)
    ["up", x] -> Up (read x)
    ["down", x] -> Down (read x)
    _ -> error $ "Invalid input: " ++ s

data State = State
  { position :: Int
  , depth :: Int
  , aim :: Int
  }

runCommands :: (State -> Command -> State) -> [Command] -> State
runCommands f = foldl f (State 0 0 0)

calcResult :: State -> Int
calcResult State{..} = position * depth

main :: IO ()
main = do
  input <- map parseCommand . lines <$> readFile "data/Day02.txt"

  let runWith f = print . calcResult . runCommands f $ input

  -- part 1
  runWith $ \state@State{..} -> \case
    Forward x -> state{position = position + x}
    Up x -> state{depth = depth - x}
    Down x -> state{depth = depth + x}

  -- part 2
  runWith $ \state@State{..} -> \case
    Forward x -> state{position = position + x, depth = depth + x * aim}
    Down x -> state{aim = aim + x}
    Up x -> state{aim = aim - x}
