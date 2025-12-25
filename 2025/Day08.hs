{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text qualified as Text

main :: IO ()
main = do
  (part1Stop, coords) <- parse . lines <$> getContents

  let states = scanl step (initCircuits coords, Nothing) (mergeOrder coords)
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
        Just ((x1, _, _), (x2, _, _)) -> x1 * x2
        Nothing -> error "last state did not have a pair of coordinates"

  printPart 2 . multiplyXs . snd $ part2State

printPart :: (Show a) => Int -> a -> IO ()
printPart part result = putStrLn $ "Part " ++ show part ++ ": " ++ show result

type Coordinate = (Int, Int, Int)

parse :: [String] -> (Int, [Coordinate])
parse = \case
  line1 : input ->
    let part1Stop = read . fst . break (== ' ') $ line1
     in (part1Stop, map parseCoord input)
  _ -> error "invalid input"
 where
  parseCoord line =
    case map Text.unpack . Text.splitOn "," . Text.pack $ line of
      [x, y, z] -> (read x, read y, read z)
      _ -> error "invalid input"

distance :: Coordinate -> Coordinate -> Double
distance (x1, y1, z1) (x2, y2, z2) = sqrt $ squared (x2 - x1) + squared (y2 - y1) + squared (z2 - z1)
 where
  squared n = fromIntegral n ^ (2 :: Int)

type CircuitId = Int
type Circuits = Map Coordinate CircuitId

initCircuits :: [Coordinate] -> Circuits
initCircuits = Map.fromList . flip zip [1 ..]

circuitSizes :: Circuits -> [Int]
circuitSizes = Map.elems . Map.fromListWith (+) . map (,1) . Map.elems

allSameCircuit :: Circuits -> Bool
allSameCircuit circuits = all (== firstCircuit) circuits
 where
  firstCircuit = snd $ Map.findMin circuits

mergeOrder :: [Coordinate] -> [(Coordinate, Coordinate)]
mergeOrder = sortOn (uncurry distance) . allPairs

step ::
  (Circuits, Maybe (Coordinate, Coordinate)) ->
  (Coordinate, Coordinate) ->
  (Circuits, Maybe (Coordinate, Coordinate))
step (circuits, _) pair@(coord1, coord2) =
  let
    circuit1 = circuits Map.! coord1
    circuit2 = circuits Map.! coord2
    circuits' =
      if circuit1 == circuit2
        then circuits
        else merge circuit1 circuit2 <$> circuits
   in
    (circuits', Just pair)
 where
  -- Update all coordinates in circuit1 to be in circuit2
  merge circuit1 circuit2 circuit = if circuit == circuit1 then circuit2 else circuit

allPairs :: [a] -> [(a, a)]
allPairs = \case
  [] -> []
  x : xs -> map (x,) xs ++ allPairs xs
