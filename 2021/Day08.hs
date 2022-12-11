{-
stack script --resolver lts-18.18
  --package containers
  --package text
  --optimize
-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Exception (evaluate)
import Control.Monad (forM)
import Data.Foldable (foldlM)
import Data.List (foldl1', permutations)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Tuple (swap)

main :: IO ()
main = do
  input <- map parsePuzzle . lines <$> readFile "data/Day08.txt"

  -- part 1
  let uniqueNumWires = [2, 4, 3, 7] -- # of wires for 1, 4, 7, and 8
  print $ length $ filter ((`elem` uniqueNumWires) . length . showPattern) $ concatMap outputPatterns input

  -- part 2
  outputVals <- forM (zip [1..] input) $ \(i, puzzle) -> do
    let output = decodeOutput puzzle

    -- debug code
    output' <- evaluate output
    putStrLn $ "[debug|" ++ show i ++ "] " ++ show output'

    return output

  print $ sum outputVals

data Puzzle = Puzzle
  { inputPatterns :: [SignalPattern]
  , outputPatterns :: [SignalPattern]
  }
  deriving (Show)

parsePuzzle :: String -> Puzzle
parsePuzzle s =
  fromMaybe (error $ "Invalid puzzle: " ++ show s) $ do
    [inputPatterns, outputPatterns] <- pure $ splitOn " | " s
    Puzzle <$> parsePatterns inputPatterns <*> parsePatterns outputPatterns
  where
    parsePatterns = mapM parsePattern . words

data WireTo a = WireTo
  { wireA :: a
  , wireB :: a
  , wireC :: a
  , wireD :: a
  , wireE :: a
  , wireF :: a
  , wireG :: a
  }
  deriving (Show, Eq, Ord)

type SignalPattern = WireTo Bool
type WireMap = WireTo SignalWire
type SignalWire = Char

blankPattern :: SignalPattern
blankPattern =
  WireTo
    { wireA = False
    , wireB = False
    , wireC = False
    , wireD = False
    , wireE = False
    , wireF = False
    , wireG = False
    }

parsePattern :: String -> Maybe SignalPattern
parsePattern = foldlM parseWire blankPattern
  where
    parseWire pat = \case
      'a' -> Just $ pat{wireA = True}
      'b' -> Just $ pat{wireB = True}
      'c' -> Just $ pat{wireC = True}
      'd' -> Just $ pat{wireD = True}
      'e' -> Just $ pat{wireE = True}
      'f' -> Just $ pat{wireF = True}
      'g' -> Just $ pat{wireG = True}
      _ -> Nothing

parsePattern' :: String -> SignalPattern
parsePattern' s = fromMaybe (error $ "Invalid pattern: " ++ show s) $ parsePattern s

showPattern :: SignalPattern -> String
showPattern WireTo{..} =
  catMaybes
    [ wireA `as` 'a'
    , wireB `as` 'b'
    , wireC `as` 'c'
    , wireD `as` 'd'
    , wireE `as` 'e'
    , wireF `as` 'f'
    , wireG `as` 'g'
    ]
  where
    as cond c = if cond then Just c else Nothing

emptyWireMap :: WireTo (Maybe SignalWire)
emptyWireMap =
  WireTo
    { wireA = Nothing
    , wireB = Nothing
    , wireC = Nothing
    , wireD = Nothing
    , wireE = Nothing
    , wireF = Nothing
    , wireG = Nothing
    }

mkAllPossibleMaps :: SignalPattern -> SignalPattern -> [WireTo (Maybe SignalWire)]
mkAllPossibleMaps inputPat outputPat
  | numWires inputPat /= numWires outputPat = []
  | otherwise = map buildWireMap (permutations outputs)
  where
    numWires WireTo{..} =
      length $ filter id [wireA, wireB, wireC, wireD, wireE, wireF, wireG]

    buildWireMap wires = compose (zipWith ($) setters wires) emptyWireMap

    compose :: [a -> a] -> a -> a
    compose = foldr (.) id

    setters =
      map snd . filter (($ inputPat) . fst) $
        [ (wireA, \x wireMap -> wireMap{wireA = Just x})
        , (wireB, \x wireMap -> wireMap{wireB = Just x})
        , (wireC, \x wireMap -> wireMap{wireC = Just x})
        , (wireD, \x wireMap -> wireMap{wireD = Just x})
        , (wireE, \x wireMap -> wireMap{wireE = Just x})
        , (wireF, \x wireMap -> wireMap{wireF = Just x})
        , (wireG, \x wireMap -> wireMap{wireG = Just x})
        ]

    outputs =
      map snd . filter (($ outputPat) . fst) $
        [ (wireA, 'a')
        , (wireB, 'b')
        , (wireC, 'c')
        , (wireD, 'd')
        , (wireE, 'e')
        , (wireF, 'f')
        , (wireG, 'g')
        ]

decodeWire :: WireMap -> SignalPattern -> SignalPattern
decodeWire WireTo{..} pat =
  parsePattern'
    . map (revMap Map.!)
    . showPattern
    $ pat
  where
    revMap =
      Map.fromList
        [ ('a', wireA)
        , ('b', wireB)
        , ('c', wireC)
        , ('d', wireD)
        , ('e', wireE)
        , ('f', wireF)
        , ('g', wireG)
        ]

decodeOutput :: Puzzle -> Int
decodeOutput puzzle@Puzzle{outputPatterns} =
  let wireMap = getWireMap puzzle
   in digitsToInt $ map (decodePatternWith wireMap) outputPatterns
  where
    decodePatternWith wireMap pat =
      case decodeWire wireMap pat `Map.lookup` patternToDigit of
        Just d -> d
        Nothing -> error $ "Could not decode pattern: " ++ show (pat, wireMap)

-- | Output is a WireMap mapping the encoded wire to the actual wire.
getWireMap :: Puzzle -> WireMap
getWireMap puzzle@Puzzle{inputPatterns} =
  findFullMap . foldl1' mergePossibleWireMaps . map patternToPossibleWireMaps $ inputPatterns
  where
    findFullMap :: [WireTo (Maybe SignalWire)] -> WireMap
    findFullMap wireMaps =
      case mapMaybe toFullMap wireMaps of
        [] -> error $ "No solution for puzzle: " ++ show puzzle
        [wireMap] -> wireMap
        maps -> error $ "Ambiguous solution: " ++ show (maps, puzzle)

    toFullMap :: WireTo (Maybe SignalWire) -> Maybe (WireTo SignalWire)
    toFullMap WireTo{..} =
      WireTo
        <$> wireA
        <*> wireB
        <*> wireC
        <*> wireD
        <*> wireE
        <*> wireF
        <*> wireG

    mergePossibleWireMaps :: [WireTo (Maybe SignalWire)] -> [WireTo (Maybe SignalWire)] -> [WireTo (Maybe SignalWire)]
    mergePossibleWireMaps wireMaps1 wireMaps2 =
      [ merged
      | wireMap1 <- wireMaps1
      , wireMap2 <- wireMaps2
      , Just merged <- pure $ mergeWireMaps wireMap1 wireMap2
      ]

    mergeWireMaps :: WireTo (Maybe SignalWire) -> WireTo (Maybe SignalWire) -> Maybe (WireTo (Maybe SignalWire))
    mergeWireMaps wireMap1 wireMap2 =
      let merge f =
            case (f wireMap1, f wireMap2) of
              (Nothing, Nothing) -> Just Nothing
              (Just x, Nothing) -> Just (Just x)
              (Nothing, Just x) -> Just (Just x)
              (Just x1, Just x2)
                | x1 == x2 -> Just (Just x1)
                | otherwise -> Nothing
       in WireTo
            <$> merge wireA
            <*> merge wireB
            <*> merge wireC
            <*> merge wireD
            <*> merge wireE
            <*> merge wireF
            <*> merge wireG

    patternToPossibleWireMaps :: SignalPattern -> [WireTo (Maybe SignalWire)]
    patternToPossibleWireMaps encodedPat =
      concatMap (mkAllPossibleMaps encodedPat) $ Map.keys patternToDigit

type Digit = Int

patternToDigit :: Map SignalPattern Digit
patternToDigit =
  Map.fromList . map swap $
    [ (0, parsePattern' "abcefg")
    , (1, parsePattern' "cf")
    , (2, parsePattern' "acdeg")
    , (3, parsePattern' "acdfg")
    , (4, parsePattern' "bcdf")
    , (5, parsePattern' "abdfg")
    , (6, parsePattern' "abdefg")
    , (7, parsePattern' "acf")
    , (8, parsePattern' "abcdefg")
    , (9, parsePattern' "abcdfg")
    ]

digitsToInt :: [Digit] -> Int
digitsToInt = foldl (\acc d -> acc * 10 + d) 0

{-- Utilities --}

splitOn :: String -> String -> [String]
splitOn sep = map Text.unpack . Text.splitOn (Text.pack sep) . Text.pack
