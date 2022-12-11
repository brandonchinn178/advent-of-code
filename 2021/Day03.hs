{-
stack script --resolver lts-18.18
-}
{-# LANGUAGE LambdaCase #-}

import Control.Arrow ((&&&))
import Data.Either (partitionEithers)
import Data.List (foldl', transpose)

main :: IO ()
main = do
  input <- map readBits . lines <$> readFile "data/Day03.txt"
  let digits = [0 .. length (head input) - 1]

  -- part 1
  let gamma = map getMostFreq $ transpose input
      eps = map flipBit gamma
  print $ toInt gamma * toInt eps

  -- part 2
  let filterUsingBitAt comp tieBreaker nums i =
        let (zs, os) = partitionByBitAt i nums
         in case compare (length zs) (length os) of
              EQ -> unBit (zs, os) tieBreaker
              res -> if res == comp then zs else os
      runRatingWith comp tieBreaker =
        foldlUntilOne (filterUsingBitAt comp tieBreaker) input digits
      o2rating = runRatingWith GT One
      co2rating = runRatingWith LT Zero
  print $ toInt o2rating * toInt co2rating

data Bit = Zero | One
  deriving (Show, Eq, Ord)

type BitNum = [Bit]

readBits :: String -> BitNum
readBits = map $ \case
  '0' -> Zero
  '1' -> One
  c -> error $ "Unknown bit: " ++ [c]

flipBit :: Bit -> Bit
flipBit Zero = One
flipBit One = Zero

unBit :: (a, a) -> Bit -> a
unBit (x, _) Zero = x
unBit (_, x) One = x

oneIf :: Bool -> Bit
oneIf True = One
oneIf False = Zero

toInt :: BitNum -> Int
toInt = foldl' (\acc bit -> unBit (0, 1) bit + 2 * acc) 0

getMostFreq :: [Bit] -> Bit
getMostFreq bits = oneIf $ uncurry (<=) $ foldl' tally (0, 0) bits
  where
    tally (zs, os) = \case
      Zero -> (zs + 1, os)
      One -> (zs, os + 1)

-- Looks at the bit at the given index in each number, then
-- returns numbers where the bit at the given position is (Zero, One)
partitionByBitAt :: Int -> [BitNum] -> ([BitNum], [BitNum])
partitionByBitAt i = partitionEithers . map toLeastOrMostFreq
  where
    toLeastOrMostFreq num =
      case num !! i of
        Zero -> Left num
        One -> Right num

foldlUntilOne :: Show b => ([b] -> a -> [b]) -> [b] -> [a] -> b
foldlUntilOne f bs as =
  case foldl' go bs as of
    [x] -> x
    res -> error $ "Expected one result, got: " ++ show res
  where
    go bs a = if length bs == 1 then bs else f bs a
