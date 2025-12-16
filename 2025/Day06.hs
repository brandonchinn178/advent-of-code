{-# LANGUAGE RecordWildCards #-}

import Data.Char (isDigit, digitToInt)
import Data.List (transpose)

main :: IO ()
main = do
  Input{..} <- parse . lines <$> getContents

  let rows = transpose $ map (map toInt . splitOn (== Border)) chars
  printPart 1 . sum . zipWith apply ops $ rows

  let cols = map (map toInt) . splitOn (all (== Border)) $ transpose chars
  printPart 2 . sum . zipWith apply ops $ cols

printPart :: (Show a) => Int -> a -> IO ()
printPart part result = putStrLn $ "Part " ++ show part ++ ": " ++ show result

data Input = Input
  { chars :: [[Chr]]
  , ops :: [Op]
  }

data Chr = Digit Int | Space | Border deriving (Eq)
data Op = ADD | MUL

parse :: [String] -> Input
parse xs =
  Input
    { chars = detectBorders . map (map toChr) $ init xs
    , ops = map parseOp . words $ last xs
    }
 where
  toChr ' ' = Space
  toChr c | isDigit c = Digit (digitToInt c)
  toChr c = error $ "Unknown char: " ++ [c]

  detectBorders =
    let go cs = if all (== Space) cs then map (const Border) cs else cs
     in transpose . map go . transpose

  parseOp "*" = MUL
  parseOp "+" = ADD
  parseOp s = error $ "Unknown op: " ++ s

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn f = foldr go [[]]
  where
    go a acc | f a = [] : acc
    go a (x:xs) = (a:x) : xs
    go _ [] = error "Unexpected empty list"

toInt :: [Chr] -> Int
toInt cs = foldl' (\acc n -> acc * 10 + n) 0 [n | Digit n <- cs]

apply :: Op -> [Int] -> Int
apply ADD = sum
apply MUL = product
