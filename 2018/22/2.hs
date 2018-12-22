-- stack --resolver lts-12.0 script
{-# OPTIONS_GHC -Weverything -Wno-implicit-prelude #-}
module Main ( main ) where
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Graph.Inductive as Graph
import qualified Data.Map as Map
import qualified Data.Set as Set

-- answer 1059 is too high

main :: IO ()
main = do
  [d, tx, ty] <- fmap (map read . words . map keepDigit) (readFile "input.txt")
  caveVar <- STM.newTVarIO (newCave (Depth d) (X tx) (Y ty))
  Just result <- STM.atomically (solve caveVar)
  print (unwrapWeight result)

solve :: STM.TVar Cave -> STM.STM (Maybe Weight)
solve caveVar = do
  Target (Coordinate (X tx) (Y ty)) <- fmap caveTarget (STM.readTVar caveVar)
  let
    xMax = 2 * tx
    yMax = 2 * ty
    xs = [0 .. xMax]
    ys = [0 .. yMax]
    coordinates = concatMap (\ x -> map (\ y -> Coordinate (X x) (Y y)) ys) xs

  Monad.forM_ coordinates (getRegionType caveVar)
  cave <- STM.readTVar caveVar

  nodesVar <- STM.newTVar Map.empty
  Monad.forM_ coordinates (\ coordinate ->
    case Map.lookup coordinate (caveRegionTypes cave) of
      Nothing -> fail "couldn't find region type"
      Just regionType -> Monad.forM_ tools (\ tool ->
        case (regionType, tool) of
          (RegionTypeRocky, Nothing) -> pure ()
          (RegionTypeWet, Just ToolTorch) -> pure ()
          (RegionTypeNarrow, Just ToolClimbingGear) -> pure ()
          _ -> STM.modifyTVar nodesVar (addNode (Node coordinate tool))))
  nodes <- STM.readTVar nodesVar

  edgesVar <- STM.newTVar Set.empty
  Monad.forM_ (Map.toList nodes) (\ (Node fromCoordinate fromTool, fromSerial) ->
    Monad.forM_ (getNeighbors fromCoordinate) (\ toCoordinate ->
      Monad.forM_ tools (\ toTool ->
        case Map.lookup (Node toCoordinate toTool) nodes of
          Nothing -> pure ()
          Just toSerial -> do
            let from = (fromTool, fromSerial)
            let to = (toTool, toSerial)
            STM.modifyTVar edgesVar (addEdge from to))))
  edges <- STM.readTVar edgesVar

  from <- maybe
    (fail "couldn't find origin node")
    (pure . unwrapSerial)
    (Map.lookup (Node (Coordinate (X 0) (Y 0)) (Just ToolTorch)) nodes)
  to <- maybe
    (fail "couldn't find target node")
    (pure . unwrapSerial)
    (Map.lookup (Node (Coordinate (X tx) (Y ty)) (Just ToolTorch)) nodes)

  pure (Graph.spLength from to (makeGraph nodes edges))

makeGraph
  :: Map.Map Node Serial
  -> Set.Set (Serial, Serial, Weight)
  -> Graph.Gr () Weight
makeGraph nodes edges = Graph.mkGraph
  (map (\ n -> (unwrapSerial n, ())) (Map.elems nodes))
  (map (\ (n, m, w) -> (unwrapSerial n, unwrapSerial m, w)) (Set.toList edges))

addNode :: Node -> Map.Map Node Serial -> Map.Map Node Serial
addNode node nodes =
  Map.insertWith (\ _ old -> old) node (Serial (Map.size nodes)) nodes

addEdge
  :: (Maybe Tool, Serial)
  -> (Maybe Tool, Serial)
  -> Set.Set (Serial, Serial, Weight)
  -> Set.Set (Serial, Serial, Weight)
addEdge (fromTool, fromSerial) (toTool, toSerial) =
  Set.insert (fromSerial, toSerial, if fromTool == toTool then 1 else 8)

tools :: [Maybe Tool]
tools =
  [ Nothing
  , Just ToolClimbingGear
  , Just ToolTorch
  ]

getGeologicIndex :: STM.TVar Cave -> Coordinate -> STM.STM GeologicIndex
getGeologicIndex caveVar coordinate = cached caveVar
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
      left <- getErosionLevel caveVar (goLeft coordinate)
      up <- getErosionLevel caveVar (goUp coordinate)
      pure (GeologicIndex (unwrapErosionLevel left * unwrapErosionLevel up)))

getErosionLevel :: STM.TVar Cave -> Coordinate -> STM.STM ErosionLevel
getErosionLevel caveVar coordinate = cached caveVar
  (Map.lookup coordinate . caveErosionLevels)
  (updateErosionLevels . Map.insert coordinate)
  (\ cave -> do
    geologicIndex <- getGeologicIndex caveVar coordinate
    pure (ErosionLevel (rem
      (unwrapGeologicIndex geologicIndex + unwrapDepth (caveDepth cave))
      20183)))

getRegionType :: STM.TVar Cave -> Coordinate -> STM.STM RegionType
getRegionType caveVar coordinate = cached caveVar
  (Map.lookup coordinate . caveRegionTypes)
  (updateRegionTypes . Map.insert coordinate)
  (\ _ -> do
    erosionLevel <- getErosionLevel caveVar coordinate
    case rem (unwrapErosionLevel erosionLevel) 3 of
      0 -> pure RegionTypeRocky
      1 -> pure RegionTypeWet
      2 -> pure RegionTypeNarrow
      _ -> fail "math stopped working")

getNeighbors :: Coordinate -> [Coordinate]
getNeighbors coordinate =
  [ goUp coordinate
  , goRight coordinate
  , goDown coordinate
  , goLeft coordinate
  ]

goUp :: Coordinate -> Coordinate
goUp coordinate =
  coordinate { coordinateY = Y (unwrapY (coordinateY coordinate) - 1) }

goRight :: Coordinate -> Coordinate
goRight coordinate =
  coordinate { coordinateX = X (unwrapX (coordinateX coordinate) + 1) }

goDown :: Coordinate -> Coordinate
goDown coordinate =
  coordinate { coordinateY = Y (unwrapY (coordinateY coordinate) + 1) }

goLeft :: Coordinate -> Coordinate
goLeft coordinate =
  coordinate { coordinateX = X (unwrapX (coordinateX coordinate) - 1) }

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

newtype Weight = Weight
  { unwrapWeight :: Double
  } deriving (Eq, Ord, Show)

instance Num Weight where
  fromInteger = Weight . fromInteger
  abs = Weight . abs . unwrapWeight
  negate = Weight . negate . unwrapWeight
  signum = Weight . signum . unwrapWeight
  x + y = Weight (unwrapWeight x + unwrapWeight y)
  x * y = Weight (unwrapWeight x * unwrapWeight y)

instance Real Weight where
  toRational = toRational . unwrapWeight

newtype Serial = Serial
  { unwrapSerial :: Int
  } deriving (Eq, Ord, Show)

data Node = Node
  { nodeCoordinate :: Coordinate
  , nodeTool :: Maybe Tool
  } deriving (Eq, Ord, Show)

data Tool
  = ToolClimbingGear
  | ToolTorch
  deriving (Eq, Ord, Show)

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
