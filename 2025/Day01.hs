{-# LANGUAGE LambdaCase #-}

main :: IO ()
main = do
  input <- map parse . lines <$> getContents

  let start = 50

  -- part 1
  let states = scanl (\a b -> (a + b) `mod` 100) start input
  printPart 1 . length . filter (== 0) $ states

  -- part 2
  let (_, count) = foldl' step (start, 0) input
  printPart 2 count

printPart :: Show a => Int -> a -> IO ()
printPart part result = putStrLn $ "Part " ++ show part ++ ": " ++ show result

parse :: String -> Int
parse = \case
  'R':s -> read s
  'L':s -> -1 * read s
  s -> error $ "Unknown input: " ++ s

step :: (Int, Int) -> Int -> (Int, Int)
step (prev, count) x = (next, count + numZeros)
  where
    next = (x + prev) `mod` 100
    numZeros = sum
      [ abs (x + prev) `quot` 100
      , if prev > 0 && x + prev < 0 then 1 else 0
      , if x + prev == 0 then 1 else 0
      ]
