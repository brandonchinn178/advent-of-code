import Data.List (groupBy, sort)
import Data.Ord (Down (..))

import Data.Time

main :: IO ()
main = withTimer $ do
  input <- lines <$> readFile "2022/data/Day01.txt"
  let elves = filter (/= [""]) $ groupBy (\a b -> not (null a || null b)) input
  case map getDown $ sort $ map (Down . sum . map read) elves of
    a:b:c:_ -> print a >> print (a + b + c)

withTimer :: IO () -> IO ()
withTimer m = do
  start <- getCurrentTime
  m
  end <- getCurrentTime
  putStrLn $ "Elapsed: " <> show (end `diffUTCTime` start)
