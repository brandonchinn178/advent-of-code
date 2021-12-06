{-
stack script --resolver lts-18.18
  --package text
-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

import Data.Text qualified as Text

main :: IO ()
main = do
  input <- map (read @Int) . splitOn "," <$> readFile "Day06.txt"

  let countTotalFishAtDay x = sum $ map (\c -> totalFishFrom c !! x) input

  -- part 1
  print $ countTotalFishAtDay 80

  -- part 2
  print $ countTotalFishAtDay 256

-- `totalFishFrom c !! x` represents the total number of active fish
-- at day `x` who descended from a single fish starting at an internal
-- counter of `c`.
--
-- Includes the original fish in the count (so `totalFishFrom c !! 0 == 1`)
-- and includes all fish birthed by fish birthed by the original fish (and so
-- on).
totalFishFrom :: Int -> [Int]
totalFishFrom c = replicate (c + 1) 1 ++ zipWith (+) totalFishFrom6 totalFishFrom8

-- memoize `totalFishFrom 6`
totalFishFrom6 :: [Int]
totalFishFrom6 = totalFishFrom 6

-- memoize `totalFishFrom 8`
totalFishFrom8 :: [Int]
totalFishFrom8 = totalFishFrom 8

{-- Utilities --}

splitOn :: String -> String -> [String]
splitOn sep = map Text.unpack . Text.splitOn (Text.pack sep) . Text.pack
