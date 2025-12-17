{-# LANGUAGE RecordWildCards #-}

import Data.Either (partitionEithers)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main = do
  Input{..} <- parse . lines <$> getContents

  let SimulationState{..} = runSimulation start splitters
  printPart 1 . length $ seenSplitters
  printPart 2 . sum . Map.elems $ beamCounts

printPart :: (Show a) => Int -> a -> IO ()
printPart part result = putStrLn $ "Part " ++ show part ++ ": " ++ show result

type Coord = (Int, Int)

data Input = Input
  { start :: Coord
  , splitters :: [Coord]
  }

parse :: [String] -> Input
parse input =
  let (starts, splitters) =
        partitionEithers
          [ coord
          | (y, line) <- zip [0 ..] input
          , (x, c) <- zip [0 ..] line
          , coord <-
              case c of
                'S' -> pure $ Left (x, y)
                '^' -> pure $ Right (x, y)
                _ -> mempty
          ]
   in Input
        { start =
            case starts of
              [x] -> x
              _ -> error $ "Unexpectedly found multiple starts: " ++ show starts
        , splitters = splitters
        }

data SimulationState = SimulationState
  { beamCounts :: Map Int Int -- Beam column -> # of beams in superposition
  , seenSplitters :: Set Coord
  }

runSimulation :: Coord -> [Coord] -> SimulationState
runSimulation start = foldl' go initialState . Map.toAscList . groupOnRow
 where
  groupOnRow = Map.fromListWith (<>) . map (\(x, y) -> (y, Set.singleton x))

  initialState =
    SimulationState
      { beamCounts = Map.singleton (fst start) 1
      , seenSplitters = Set.empty
      }

  go SimulationState{..} (row, splitterCols) =
    let (beams', newSplitters) =
          unzip
            [ (map (,beamCount) newBeamCols, splitter)
            | (beamCol, beamCount) <- Map.toList beamCounts
            , let isSplit = beamCol `Set.member` splitterCols
            , let newBeamCols = if isSplit then [beamCol - 1, beamCol + 1] else [beamCol]
            , let splitter = if isSplit then Set.singleton (beamCol, row) else Set.empty
            ]
     in SimulationState
          { beamCounts = Map.fromListWith (+) $ concat beams'
          , seenSplitters = seenSplitters <> Set.unions newSplitters
          }
