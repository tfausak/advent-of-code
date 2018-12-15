-- stack --resolver lts-12.0 script
module Main ( main ) where
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as Set


main :: IO ()
main = do
  input <- readFile "input.txt"
  initialArea <- either fail pure (parseArea input)
  let ((round_, area), _) = head (dropWhile
        (not . hasEnded . snd . fst)
        (simulate initialRound initialArea))
  printArea area
  putStrLn ("Completed " <> renderRound round_ <> " full rounds.")
  let units = Map.elems (areaUnits area)
  mapM_ (putStrLn . renderUnit) units
  let health = sum (map (unwrapHealth . unitHealth) units)
  print health
  print ((unwrapRound round_ - 1) * health) -- TODO: Off by one :(


printArea :: Area -> IO ()
printArea area = maybe
  (fail ("invalid area: " <> show area))
  putStrLn
  (renderArea area)


renderArea :: Area -> Maybe String
renderArea area = do
  let
    units = areaUnits area
    walls = areaWalls area
    xs = Set.map pointX walls
    ys = Set.map pointY walls
  minX <- Set.lookupMin xs
  maxX <- Set.lookupMax xs
  minY <- Set.lookupMin ys
  maxY <- Set.lookupMax ys
  pure (List.intercalate "\n" (map
    (\y -> map
      (\x -> let point = Point x y in case Map.lookup point units of
        Just unit -> case unitRace unit of
          RaceElf -> 'E'
          RaceGoblin -> 'G'
        Nothing -> if Set.member point walls then '#' else '.')
      [minX .. maxX])
    [minY .. maxY]))


simulate :: Round -> Area -> [((Round, Area), [String])]
simulate round_ area = let
  nextArea :: Area
  logs :: [String]
  (nextArea, logs) = Writer.runWriter (simulateRound area)
  nextRound = overRound (+ 1) round_
  in ((nextRound, nextArea), logs) : simulate nextRound nextArea


hasEnded :: Area -> Bool
hasEnded = homogeneous . map unitRace . Map.elems . areaUnits


homogeneous :: Eq a => [a] -> Bool
homogeneous list = case list of
  [] -> True
  first : rest -> all (== first) rest


newtype Round = Round
  { unwrapRound :: Int
  } deriving (Show)


initialRound :: Round
initialRound = Round 0


overRound :: (Int -> Int) -> Round -> Round
overRound f = Round . f. unwrapRound


renderRound :: Round -> String
renderRound = show . unwrapRound


simulateRound :: Monad m => Area -> m Area
simulateRound area = Foldable.foldlM (\a p -> pure $ simulateTurn a p) area (Map.keys (areaUnits area))


simulateTurn :: Area -> Point -> Area
simulateTurn area point = case Map.lookup point (areaUnits area) of
  Nothing -> area
  Just unit -> let
    targets = findTargets (unitRace unit) (areaUnits area)
    newPoint = if any (flip Map.member targets) (neighbors point) then point else let
      points = filter (not . isOccupied area) (Set.toAscList (Set.unions
        (map neighbors (Map.keys targets))))
      in Maybe.fromMaybe point (findNextStep area point points)
    adjacentTargets = Map.toList (Map.restrictKeys targets (neighbors newPoint))
    maybeTarget = fmap fst (minimumOn (\(p, u) -> (unitHealth u, p)) adjacentTargets)
    in updateUnits
      ( Map.filter isAlive
      . maybe id (Map.adjust (attack unit)) maybeTarget
      . Map.insert newPoint unit
      . Map.delete point
      ) area


attack :: Unit -> Unit -> Unit
attack = updateHealth . overHealth . subtract . unwrapPower . unitPower


isAlive :: Unit -> Bool
isAlive = (> 0) . unwrapHealth . unitHealth


findNextStep
  :: Area
  -> Point -- where i am right now
  -> [Point] -- points that are adjacent to all of my enemies
  -> Maybe Point
findNextStep area current points = do
  target <- do
    -- first compute the distance from my current position to every other point in the area
    let
      crumbs = breadcrumbs area (Seq.singleton current) (Map.singleton current current)
      withCost point = case computeCost current point crumbs of
        Nothing -> Nothing
        Just cost -> Just (cost, point)
    -- then find the point that's closest to me
    fmap snd (minimumMaybe (Maybe.mapMaybe withCost points))
  -- now that i know which point i'm heading toward, i want to find the first step to take towards it
  let
    crumbs = breadcrumbs area (Seq.singleton target) (Map.singleton target target)
    withCost point = case computeCost target point crumbs of
      Nothing -> Nothing
      Just cost -> Just (cost, point)
  fmap snd (minimumMaybe (Maybe.mapMaybe withCost (Set.toList (neighbors current))))


minimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumBy f xs = if null xs then Nothing else Just (List.minimumBy f xs)


minimumMaybe :: Ord a => [a] -> Maybe a
minimumMaybe = minimumOn id


minimumOn :: Ord b => (a -> b) -> [a] -> Maybe a
minimumOn = minimumBy . Ord.comparing


computeCost :: Point -> Point -> Map.Map Point Point -> Maybe Cost
computeCost start goal crumbs = if goal == start
  then Just (Cost 0)
  else case Map.lookup goal crumbs of
    Nothing -> Nothing
    Just next -> fmap (overCost (+ 1)) (computeCost start next crumbs)


breadcrumbs :: Area -> Seq.Seq Point -> Map.Map Point Point -> Map.Map Point Point
breadcrumbs area frontier cameFrom = case Seq.viewl frontier of
  Seq.EmptyL -> cameFrom
  current Seq.:< rest -> let
    (newFrontier, newCameFrom) = foldr
      (\next (s, m) -> if Map.member next m || isOccupied area next
        then (s, m)
        else (s Seq.|> next, Map.insert next current m))
      (rest, cameFrom)
      (neighbors current)
    in breadcrumbs area newFrontier newCameFrom


newtype Cost = Cost
  { unwrapCost :: Int
  } deriving (Eq, Ord, Show)


overCost :: (Int -> Int) -> Cost -> Cost
overCost f = Cost . f . unwrapCost


isOccupied :: Area -> Point -> Bool
isOccupied area point
  = Set.member point (areaWalls area)
  || Map.member point (areaUnits area)


neighbors :: Point -> Set.Set Point
neighbors point = Set.fromList
  [ updateX (overX (+ 1)) point
  , updateX (overX (subtract 1)) point
  , updateY (overY (+ 1)) point
  , updateY (overY (subtract 1)) point
  ]


findTargets :: Race -> Units -> Units
findTargets race = Map.filter ((/= race) . unitRace)


data Area = Area
  { areaUnits :: Units
  , areaWalls :: Walls
  } deriving (Show)


parseArea :: String -> Either String Area
parseArea
  = fmap toArea
  . traverse (pointed parseSquare)
  . withPoints
  . lines


toArea :: [(Point, Square)] -> Area
toArea =
  snd . foldr addSquare (Serial 1, Area Map.empty Set.empty)


addSquare :: (Point, Square) -> (Serial, Area) -> (Serial, Area)
addSquare (point, square) = case square of
  SquareElf -> addUnit RaceElf point
  SquareGoblin -> addUnit RaceGoblin point
  SquareOpen -> id
  SquareWall -> addWall point


addUnit :: Race -> Point -> (Serial, Area) -> (Serial, Area)
addUnit race point (serial, area) =
  let (newSerial, unit) = makeUnit serial race
  in (newSerial, updateUnits (Map.insert point unit) area)


addWall :: Point -> (serial, Area) -> (serial, Area)
addWall = fmap . updateWalls . Set.insert


updateUnits :: (Units -> Units) -> Area -> Area
updateUnits f area = area { areaUnits = f (areaUnits area) }


updateWalls :: (Walls -> Walls) -> Area -> Area
updateWalls f area = area { areaWalls = f (areaWalls area) }


type Units = Map.Map Point Unit


data Unit = Unit
  { unitSerial :: Serial
  , unitRace :: Race
  , unitPower :: Power
  , unitHealth :: Health
  } deriving (Show)


makeUnit :: Serial -> Race -> (Serial, Unit)
makeUnit serial race =
  (overSerial (+ 1) serial, Unit serial race (Power 3) (Health 200))


renderUnit :: Unit -> String
renderUnit unit
  = renderRace (unitRace unit)
  <> "#"
  <> renderSerial (unitSerial unit)
  <> "-"
  <> renderPower (unitPower unit)
  <> "/"
  <> renderHealth (unitHealth unit)


updateHealth :: (Health -> Health) -> Unit -> Unit
updateHealth f unit = unit { unitHealth = f (unitHealth unit) }


newtype Serial = Serial
  { unwrapSerial :: Int
  } deriving (Show)


overSerial :: (Int -> Int) -> Serial -> Serial
overSerial f = Serial . f . unwrapSerial


renderSerial :: Serial -> String
renderSerial = show . unwrapSerial


data Race
  = RaceElf
  | RaceGoblin
  deriving (Eq, Show)


renderRace :: Race -> String
renderRace race = case race of
  RaceElf -> "Elf"
  RaceGoblin -> "Goblin"


newtype Health = Health
  { unwrapHealth :: Int
  } deriving (Eq, Ord, Show)


renderHealth :: Health -> String
renderHealth = show . unwrapHealth


overHealth :: (Int -> Int) -> Health -> Health
overHealth f = Health . f . unwrapHealth


newtype Power = Power
  { unwrapPower :: Int
  } deriving (Show)


renderPower :: Power -> String
renderPower = show . unwrapPower


type Walls = Set.Set Point


data Square
  = SquareElf
  | SquareGoblin
  | SquareOpen
  | SquareWall
  deriving (Show)


parseSquare :: Char -> Either String Square
parseSquare char = case char of
  '.' -> Right SquareOpen
  '#' -> Right SquareWall
  'E' -> Right SquareElf
  'G' -> Right SquareGoblin
  _ -> Left ("unknown square: " <> show char)


data Point = Point
  { pointX :: X
  , pointY :: Y
  } deriving (Eq, Show)


instance Ord Point where
  compare p1 p2 = Ord.comparing pointY p1 p2 <> Ord.comparing pointX p1 p2


updateX :: (X -> X) -> Point -> Point
updateX f point = point { pointX = f (pointX point) }


updateY :: (Y -> Y) -> Point -> Point
updateY f point = point { pointY = f (pointY point) }


pointed :: (a -> Either String b) -> (Point, a) -> Either String (Point, b)
pointed parse (point, input) = case parse input of
  Left message -> Left ("at " <> renderPoint point <> ": " <> message)
  Right output -> Right (point, output)


renderPoint :: Point -> String
renderPoint point
  = "<"
  <> renderX (pointX point)
  <> ","
  <> renderY (pointY point)
  <> ">"


withPoints :: [[a]] -> [(Point, a)]
withPoints
  = concatMap (\(y, as) -> map (\(x, a) -> (Point x y, a)) (withXs as))
  . withYs


newtype X = X
  { unwrapX :: Int
  } deriving (Eq, Ord, Show)


instance Enum X where
  fromEnum = unwrapX
  toEnum = X


overX :: (Int -> Int) -> X -> X
overX f = X . f . unwrapX


withXs :: [a] -> [(X, a)]
withXs = zip (map X [0 ..])


renderX :: X -> String
renderX = show . unwrapX


newtype Y = Y
  { unwrapY :: Int
  } deriving (Eq, Ord, Show)


instance Enum Y where
  fromEnum = unwrapY
  toEnum = Y


overY :: (Int -> Int) -> Y -> Y
overY f = Y . f . unwrapY


withYs :: [a] -> [(Y, a)]
withYs = zip (map Y [0 ..])


renderY :: Y -> String
renderY = show . unwrapY
