{-
stack script --resolver lts-18.18
  --package text
-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

import Data.Text qualified as Text

main :: IO ()
main = do
  input <- map (read @Int) . splitOn "," <$> readFile "data/Day07.txt"

  -- part 1
  print $ getBestFuelCostWith id input

  -- part 2
  print $ getBestFuelCostWith sumTo input

getBestFuelCostWith :: (Int -> Int) -> [Int] -> Int
getBestFuelCostWith f xs = minimum $ map fuelCostTo [0 .. maximum xs]
  where
    fuelCostTo pos = sum $ map (f . abs . subtract pos) xs

sumTo :: Int -> Int
sumTo n = (n * (n + 1)) `div` 2

{-- Utilities --}

splitOn :: String -> String -> [String]
splitOn sep = map Text.unpack . Text.splitOn (Text.pack sep) . Text.pack
