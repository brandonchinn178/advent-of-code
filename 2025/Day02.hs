{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

main :: IO ()
main = do
  input <- map parse . Text.splitOn "," . Text.strip <$> Text.getContents
  printPart 1 . sum . filter isInvalid1 . concatMap expand $ input
  printPart 2 . sum . filter isInvalid2 . concatMap expand $ input

printPart :: Show a => Int -> a -> IO ()
printPart part result = putStrLn $ "Part " ++ show part ++ ": " ++ show result

parse :: Text -> (Integer, Integer)
parse s =
  case map (read . Text.unpack) $ Text.splitOn "-" s of
    [a, b] -> (a, b)
    _ -> error $ "Invalid input: " <> show s

expand :: (Integer, Integer) -> [Integer]
expand (a, b) = [a .. b]

isInvalid1 :: Integer -> Bool
isInvalid1 n = (digits `mod` 2 == 0) && (upper == lower)
  where
    digits = numDigits n
    (upper, lower) = n `divMod` (10 ^ (digits `div` 2))

isInvalid2 :: Integer -> Bool
isInvalid2 n =
  or
    [ (take digits . cycle . take len) s == s
    | len <- factors digits
    ]
  where
    s = show n
    digits = length s

factors :: Int -> [Int]
factors n = filter (\x -> n `mod` x == 0) [1 .. n - 1]

numDigits :: Integer -> Integer
numDigits = (+ 1) . floor @Double . logBase 10 . fromIntegral
