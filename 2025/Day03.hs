{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.Char (digitToInt)
import Data.List (sortOn)
import Data.Ord (Down (..))

main :: IO ()
main = do
  input <- map parse . lines <$> getContents
  putStrLn . ("Part 1: " ++) . show . sum . map (getJoltage 2) $ input
  putStrLn . ("Part 2: " ++) . show . sum . map (getJoltage 12) $ input

parse :: String -> [Int]
parse = map digitToInt

getJoltage :: Int -> [Int] -> Integer
getJoltage = go 0
  where
    go !acc 0 _ = acc
    go !acc digitsRemaining digits =
      let numDigits = length digits
          candidates = take (numDigits - digitsRemaining + 1) digits
          (maxDigitPos, maxDigit) = head . sortOn (Down . snd) . zip [1 ..] $ candidates
       in go
            (acc * 10 + fromIntegral maxDigit)
            (digitsRemaining - 1)
            (drop maxDigitPos digits)
