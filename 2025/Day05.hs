{-# LANGUAGE RecordWildCards #-}

import Data.Map (Map)
import Data.Map qualified as Map
import Data.List (sort)

main :: IO ()
main = do
  Input{..} <- parse . lines <$> getContents

  printPart 1 . length $ filter (isInAnyInterval intervals) targets
  printPart 2 $ getCardinality intervals

printPart :: (Show a) => Int -> a -> IO ()
printPart part result = putStrLn $ "Part " ++ show part ++ ": " ++ show result

type ID = Integer

data Input = Input
  { intervals :: Intervals
  , targets :: [ID]
  }

parse :: [String] -> Input
parse input =
  let (ranges, ids) = span (not . null) input
   in Input
        { intervals = toIntervals $ map parseRange ranges
        , targets = map read $ drop 1 ids
        }
 where
  parseRange line =
    let (s1, s2) = span (/= '-') line
     in (read s1, read $ drop 1 s2)

-- Maps lower end of each interval to upper end
newtype Intervals = Intervals (Map ID [ID])

toIntervals :: [(ID, ID)] -> Intervals
toIntervals = Intervals . Map.fromListWith (++) . map (fmap (: []))

isInAnyInterval :: Intervals -> ID -> Bool
isInAnyInterval (Intervals m) x = any (any (>= x)) . getKeysLeq x $ m

getKeysLeq :: (Ord k) => k -> Map k a -> Map k a
getKeysLeq k m = fst (Map.split k m) <> maybe mempty (Map.singleton k) (Map.lookup k m)

getCardinality :: Intervals -> Integer
getCardinality (Intervals m) = fst $ foldl' go (0, -1) [(lo, hi) | (lo, his) <- Map.toAscList m, hi <- sort his]
  where
    go (acc, start) (lo, hi) =
      let newItems
            | start >= hi = 0
            | start >= lo = hi - start
            | otherwise = hi - lo + 1
       in (acc + newItems, max start hi)
