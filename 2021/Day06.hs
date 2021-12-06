{-
stack script --resolver lts-18.18
  --package text
-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

import Data.List (iterate')
import Data.Text qualified as Text

main :: IO ()
main = do
  input <- map (read @Int) . splitOn "," <$> readFile "Day06.txt"

  -- part 1
  print $ length $ (!! 80) $ iterate' tick input

tick :: [Int] -> [Int]
tick = concatMap tickFish
  where
    tickFish x = if x == 0 then [6, 8] else [x - 1]

{-- Utilities --}

splitOn :: String -> String -> [String]
splitOn sep = map Text.unpack . Text.splitOn (Text.pack sep) . Text.pack
