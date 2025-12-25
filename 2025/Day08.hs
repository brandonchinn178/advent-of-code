{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoFieldSelectors #-}

import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text qualified as Text

main :: IO ()
main = do
  (part1Stop, points) <- parse . lines <$> getContents

  let states = scanl step (initCircuitMap points, Nothing) (mergeOrder points)
      part1State =
        case drop part1Stop states of
          s : _ -> s
          [] -> error "states did not reach desired number of connections"

  printPart 1 . product . take 3 . sortOn Down . circuitSizes . fst $ part1State

  let part2State =
        case dropWhile (not . allSameCircuit . fst) states of
          s : _ -> s
          [] -> error "states did not reach one unified circuit"
      multiplyXs = \case
        Just (p1, p2) -> p1.x * p2.x
        Nothing -> error "last state did not have a pair of coordinates"

  printPart 2 . multiplyXs . snd $ part2State

printPart :: (Show a) => Int -> a -> IO ()
printPart part result = putStrLn $ "Part " ++ show part ++ ": " ++ show result

data Point = Point
  { id :: !Int
  , x :: !Int
  , y :: !Int
  , z :: !Int
  }
  deriving (Eq, Ord)

parse :: [String] -> (Int, [Point])
parse = \case
  line1 : input ->
    let part1Stop = read . fst . break (== ' ') $ line1
     in (part1Stop, zipWith parsePoint input [0 ..])
  _ -> error "invalid input"
 where
  parsePoint line pointId =
    case map Text.unpack . Text.splitOn "," . Text.pack $ line of
      [x, y, z] ->
        Point
          { id = pointId
          , x = read x
          , y = read y
          , z = read z
          }
      _ -> error "invalid input"

-- Only care about relative distance, can avoid the sqrt
distance :: Point -> Point -> Int
distance p1 p2 = squared (p2.x - p1.x) + squared (p2.y - p1.y) + squared (p2.z - p1.z)
 where
  squared n = fromIntegral $ n * n

type CircuitMap = UnionFind Point

initCircuitMap :: [Point] -> CircuitMap
initCircuitMap = newUnionFind

circuitSizes :: CircuitMap -> [Int]
circuitSizes = getSizes

allSameCircuit :: CircuitMap -> Bool
allSameCircuit circuitMap =
  case getRoots circuitMap of
    [_] -> True
    _ -> False

mergeOrder :: [Point] -> [(Point, Point)]
mergeOrder = sortOn (uncurry distance) . allPairs

allPairs :: [a] -> [(a, a)]
allPairs = \case
  [] -> []
  x : xs -> map (x,) xs ++ allPairs xs

type Connection = (Point, Point)

step :: (CircuitMap, Maybe Connection) -> Connection -> (CircuitMap, Maybe Connection)
step (circuitMap, _) connection@(point1, point2) =
  let
    circuit1 = getRoot point1 circuitMap
    circuit2 = getRoot point2 circuitMap
    circuitMap' =
      if circuit1 == circuit2
        then circuitMap
        else mergeUnionFind circuit1 circuit2 circuitMap
   in
    (circuitMap', Just connection)

data UnionFind k = UnionFind
  { fromKey :: k -> Int
  , toKey :: Int -> k
  , nodes :: IntMap (Int, Int) -- (index of parent, size)
  }

newUnionFind :: (Ord k) => [k] -> UnionFind k
newUnionFind keys =
  UnionFind
    { fromKey = (indexer Map.!)
    , toKey = (keys !!)
    , nodes = IntMap.fromList [(i, (i, 1)) | (_, i) <- zip keys [0 ..]]
    }
 where
  indexer = Map.fromList $ zip keys [0 ..]

mergeUnionFind :: k -> k -> UnionFind k -> UnionFind k
mergeUnionFind k1 k2 uf = uf{nodes = go uf.nodes}
 where
  go nodes =
    let
      (root1, size1) = getNode' (uf.fromKey k1) uf
      (root2, size2) = getNode' (uf.fromKey k2) uf
      newData = (if size1 > size2 then root1 else root2, size1 + size2)
     in
      IntMap.insert root1 newData
        . IntMap.insert root2 newData
        $ nodes

getRoot :: k -> UnionFind k -> k
getRoot k = fst . getNode k

-- | Get the root node and the size of the set containing the given key.
getNode :: k -> UnionFind k -> (k, Int)
getNode k uf = first uf.toKey . flip getNode' uf . uf.fromKey $ k

-- | Get the root node and the size of the set containing the given key.
getNode' :: Int -> UnionFind k -> (Int, Int)
getNode' k uf = go k
 where
  go i =
    let (parent, size) = uf.nodes IntMap.! i
     in if parent == i
          then (i, size)
          else go parent

getSizes :: UnionFind k -> [Int]
getSizes = map snd . getRoots

getRoots :: UnionFind k -> [(k, Int)]
getRoots uf =
  [ (uf.toKey i, size)
  | (i, (parent, size)) <- IntMap.toList uf.nodes
  , parent == i
  ]
