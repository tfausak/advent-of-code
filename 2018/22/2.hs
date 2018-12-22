-- stack --resolver lts-12.0 script
{-# OPTIONS_GHC -Weverything -Wno-implicit-prelude #-}
module Main ( main ) where
import qualified Control.Concurrent.STM as STM
import qualified Data.Char as Char
import qualified Data.Map as Map

main :: IO ()
main = do
  [d, tx, ty] <- fmap (map read . words . map keepDigit) (readFile "input.txt")
  caveRef <- STM.newTVarIO (newCave (Depth d) (X tx) (Y ty))
  STM.atomically (mapM_
    (\ y -> mapM_
      (\ x -> getRegionType caveRef (Coordinate (X x) (Y y)))
      [0 .. tx])
    [0 .. ty])
  putStr . render =<< STM.readTVarIO caveRef

render :: Cave -> String
render cave =
  let Target (Coordinate (X tx) (Y ty)) = caveTarget cave
  in unlines (map
    (\ y -> map
      (\ x -> case Map.lookup (Coordinate (X x) (Y y)) (caveRegionTypes cave) of
        Nothing -> '?'
        Just regionType -> case regionType of
          RegionTypeRocky -> '.'
          RegionTypeWet -> '='
          RegionTypeNarrow -> '|')
      [0 .. tx])
    [0 .. ty])

getGeologicIndex :: STM.TVar Cave -> Coordinate -> STM.STM GeologicIndex
getGeologicIndex caveRef coordinate = cached caveRef
  (Map.lookup coordinate . caveGeologicIndices)
  (updateGeologicIndices . Map.insert coordinate)
  (\ cave -> do
    if coordinate == mouth then
      pure (GeologicIndex 0)
    else if coordinate == unwrapTarget (caveTarget cave) then
      pure (GeologicIndex 0)
    else if unwrapY (coordinateY coordinate) == 0 then
      pure (GeologicIndex (unwrapX (coordinateX coordinate) * 16807))
    else if unwrapX (coordinateX coordinate) == 0 then
      pure (GeologicIndex (unwrapY (coordinateY coordinate) * 48271))
    else do
      left <- getErosionLevel caveRef (goLeft coordinate)
      up <- getErosionLevel caveRef (goUp coordinate)
      pure (GeologicIndex (unwrapErosionLevel left * unwrapErosionLevel up)))

getErosionLevel :: STM.TVar Cave -> Coordinate -> STM.STM ErosionLevel
getErosionLevel caveRef coordinate = cached caveRef
  (Map.lookup coordinate . caveErosionLevels)
  (updateErosionLevels . Map.insert coordinate)
  (\ cave -> do
    geologicIndex <- getGeologicIndex caveRef coordinate
    pure (ErosionLevel (rem
      (unwrapGeologicIndex geologicIndex + unwrapDepth (caveDepth cave))
      20183)))

getRegionType :: STM.TVar Cave -> Coordinate -> STM.STM RegionType
getRegionType caveRef coordinate = cached caveRef
  (Map.lookup coordinate . caveRegionTypes)
  (updateRegionTypes . Map.insert coordinate)
  (\ _ -> do
    erosionLevel <- getErosionLevel caveRef coordinate
    case rem (unwrapErosionLevel erosionLevel) 3 of
      0 -> pure RegionTypeRocky
      1 -> pure RegionTypeWet
      2 -> pure RegionTypeNarrow
      _ -> fail "impossible")

goLeft :: Coordinate -> Coordinate
goLeft coordinate =
  coordinate { coordinateX = X (unwrapX (coordinateX coordinate) - 1) }

goUp :: Coordinate -> Coordinate
goUp coordinate =
  coordinate { coordinateY = Y (unwrapY (coordinateY coordinate) - 1) }

cached
  :: STM.TVar Cave
  -> (Cave -> Maybe value)
  -> (value -> Cave -> Cave)
  -> (Cave -> STM.STM value)
  -> STM.STM value
cached var get set action = do
  cache <- STM.readTVar var
  case get cache of
    Just value -> pure value
    Nothing -> do
      value <- action cache
      STM.modifyTVar var (set value)
      pure value

updateGeologicIndices :: (GeologicIndices -> GeologicIndices) -> Cave -> Cave
updateGeologicIndices update cave =
  cave { caveGeologicIndices = update (caveGeologicIndices cave) }

updateErosionLevels :: (ErosionLevels -> ErosionLevels) -> Cave -> Cave
updateErosionLevels update cave =
  cave { caveErosionLevels = update (caveErosionLevels cave) }

updateRegionTypes :: (RegionTypes -> RegionTypes) -> Cave -> Cave
updateRegionTypes update cave =
  cave { caveRegionTypes = update (caveRegionTypes cave) }

mouth :: Coordinate
mouth = Coordinate (X 0) (Y 0)

newCave :: Depth -> X -> Y -> Cave
newCave d x y = Cave d (Target (Coordinate x y)) Map.empty Map.empty Map.empty

keepDigit :: Char -> Char
keepDigit char = if Char.isDigit char then char else ' '

data Cave = Cave
  { caveDepth :: Depth
  , caveTarget :: Target
  , caveGeologicIndices :: GeologicIndices
  , caveErosionLevels :: ErosionLevels
  , caveRegionTypes :: RegionTypes
  } deriving (Eq, Ord, Show)

newtype Depth = Depth
  { unwrapDepth :: Int
  } deriving (Eq, Ord, Show)

newtype Target = Target
  { unwrapTarget :: Coordinate
  } deriving (Eq, Ord, Show)

data Coordinate = Coordinate
  { coordinateX :: X
  , coordinateY :: Y
  } deriving (Eq, Ord, Show)

newtype X = X
  { unwrapX :: Int
  } deriving (Eq, Ord, Show)

newtype Y = Y
  { unwrapY :: Int
  } deriving (Eq, Ord, Show)

type GeologicIndices
  = Map.Map Coordinate GeologicIndex

type ErosionLevels
  = Map.Map Coordinate ErosionLevel

type RegionTypes
  = Map.Map Coordinate RegionType

newtype GeologicIndex = GeologicIndex
  { unwrapGeologicIndex :: Int
  } deriving (Eq, Ord, Show)

newtype ErosionLevel = ErosionLevel
  { unwrapErosionLevel :: Int
  } deriving (Eq, Ord, Show)

data RegionType
  = RegionTypeRocky
  | RegionTypeWet
  | RegionTypeNarrow
  deriving (Eq, Ord, Show)
