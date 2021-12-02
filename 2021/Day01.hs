{-
stack script --resolver lts-18.18
-}
{-# LANGUAGE TypeApplications #-}

main :: IO ()
main = do
  input <- map (read @Int) . lines <$> readFile "Day01.txt"

  -- part 1
  print $ getNumIncreases input

  -- part 2
  let inputWindows = zipWith3 (\a b c -> a + b + c) input (tail input) (tail $ tail input)
  print $ getNumIncreases inputWindows

getNumIncreases :: Ord a => [a] -> Int
getNumIncreases xs = length . filter (uncurry (>)) $ zip (tail xs) xs
