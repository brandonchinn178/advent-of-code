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

hasCycle :: Integer -> Int -> Bool
hasCycle n size =
  size > 0 &&
  digits `mod` size == 0 &&
  (take digits . cycle . take size) s == s
  where
    s = show n
    digits = length s

isInvalid1 :: Integer -> Bool
isInvalid1 n = hasCycle n (digits `div` 2)
  where
    digits = length $ show n

isInvalid2 :: Integer -> Bool
isInvalid2 n = any (hasCycle n) [1 .. digits - 1]
  where
    digits = length $ show n
