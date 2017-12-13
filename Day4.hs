module Main where

import Utils

import Data.List (tails)

getPuzzle :: IO [[String]]
getPuzzle = (map words . lines) <$> readFile "day4.txt"

noDuplicates :: [String] -> Bool
noDuplicates phrase = all (uncurry (/=))
  [ (a,b)
  | (a:bs) <- tails phrase, b <- bs
  ]

count :: ([String] -> Bool) -> [[String]] -> Int
count f = length . filter f

main :: IO ()
main = do
  part 1 $ do
    check "noDuplicates" noDuplicates
      [ (["aa", "bb", "cc", "dd", "ee"], True)
      , (["aa", "bb", "cc", "dd", "aa"], False)
      , (["aa", "bb", "cc", "dd", "aaa"], True)
      ]
    check "count" (count noDuplicates)
      [ ([["a", "b"], ["a", "a"], ["c", "a"]], 2)
      ]
    getPuzzle >>= solve . count noDuplicates
