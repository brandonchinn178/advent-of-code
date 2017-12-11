module Main where

import Data.List (tails)

getPuzzle :: IO [[Int]]
getPuzzle = (map (map read . words) . lines) <$> readFile "day2.txt"

getCheckSum :: ([Int] -> Int) -> [[Int]] -> Int
getCheckSum getRowCheckSum = sum . map getRowCheckSum

part1Check row = maximum row - minimum row
part2Check row = go [(a,b) | (a:bs) <- tails row, b <- bs]
  where
    go ((a, b):rest) =
      let
        high = max a b
        low = min a b
      in if high `mod` low == 0
        then high `div` low
        else go rest
    go _ = error "invalid row"

main :: IO ()
main = do
  putStrLn "* Part 1:"
  check part1Check [[5,1,9,5],[7,5,3],[2,4,6,8]]
  getPuzzle >>= check part1Check

  putStrLn "* Part 2:"
  check part2Check [[5,9,2,8],[9,4,7,3],[3,8,6,5]]
  getPuzzle >>= check part2Check
  where
    check rowCheck input = do
      putStrLn "***"
      putStrLn $ "Testing: " ++ show input
      print $ getCheckSum rowCheck input
