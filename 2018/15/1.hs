-- stack --resolver lts-12.0 script
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Data.Set as Set


main :: IO ()
main = do
  input <- readFile "input.txt"
  initialArea <- either fail pure (parseArea input)
  printArea initialArea
  mapM_
    (\((_, area), logs) -> do
      mapM_ putStrLn logs
      printArea area)
    (take 1 (simulate initialRound initialArea))


printArea :: Area -> IO ()
printArea area = maybe
  (fail ("invalid area: " <> show area))
  putStrLn
  (renderArea area)


renderArea :: Area -> Maybe String
renderArea area = do
  let
    units = unwrapUnits (areaUnits area)
    walls = unwrapWalls (areaWalls area)
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
  (nextArea, logs) = Writer.runWriter (simulateRound round_ area)
  nextRound = overRound (+ 1) round_
  in ((nextRound, nextArea), logs) : simulate nextRound nextArea


hasEnded :: Area -> Bool
hasEnded
  = (== 1)
  . length
  . List.group
  . map unitRace
  . Map.elems
  . unwrapUnits
  . areaUnits


newtype Round = Round
  { unwrapRound :: Int
  } deriving (Show)


initialRound :: Round
initialRound = Round 0


overRound :: (Int -> Int) -> Round -> Round
overRound f = Round . f. unwrapRound


renderRound :: Round -> String
renderRound = show . unwrapRound


simulateRound :: Round -> Area -> Writer.Writer [String] Area
simulateRound round_ area = do
  Writer.tell ["Starting round " <> renderRound round_ <> "."]
  Foldable.foldlM simulateTurn area (Map.keys (unwrapUnits (areaUnits area)))


simulateTurn :: Area -> Point -> Writer.Writer [String] Area
simulateTurn area point =
  case Map.lookup point (unwrapUnits (areaUnits area)) of
    Nothing -> do
      Writer.tell ["Failed to find unit at " <> renderPoint point <> "!"]
      pure area
    Just unit -> do
      Writer.tell ["Starting turn at " <> renderPoint point <> " with " <> renderUnit unit <> "."]
      let targets = findTargets (unitRace unit) (areaUnits area)
      let adjacentTargets = overUnits (flip Map.restrictKeys (adjacentPoints point)) targets
      if Map.null (unwrapUnits adjacentTargets)
        then do
          Writer.tell ["Found " <> pluralize "target" (Map.size (unwrapUnits targets)) <> ": " <> List.intercalate ", " (map (\(p, u) -> renderUnit u <> " at " <> renderPoint p) (Map.toAscList (unwrapUnits targets))) <> "."]
          let points = filter (not . isOccupied area) (Set.toAscList (Set.unions
                (map adjacentPoints (Map.keys (unwrapUnits targets)))))
          Writer.tell ["Found " <> pluralize "point" (length points) <> " in range: " <> List.intercalate ", " (map renderPoint points)]
          pure area -- TODO: step toward nearest reachable target
        else do
          Writer.tell ["Found " <> pluralize "adjacent target" (Map.size (unwrapUnits adjacentTargets)) <> ": " <> List.intercalate ", " (map (\(p, u) -> renderUnit u <> " at " <> renderPoint p) (Map.toAscList (unwrapUnits adjacentTargets))) <> "."]
          pure area -- TODO: fight weakest enemy


isOccupied :: Area -> Point -> Bool
isOccupied area point
  = Set.member point (unwrapWalls (areaWalls area))
  || Map.member point (unwrapUnits (areaUnits area))


adjacentPoints :: Point -> Set.Set Point
adjacentPoints point = Set.fromList
  [ updateX (overX (+ 1)) point
  , updateX (overX (subtract 1)) point
  , updateY (overY (+ 1)) point
  , updateY (overY (subtract 1)) point
  ]


findTargets :: Race -> Units -> Units
findTargets race = overUnits (Map.filter ((/= race) . unitRace))


pluralize :: String -> Int -> String
pluralize word count
  = show count
  <> " "
  <> word
  <> if count == 1 then "" else "s"


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
  snd . foldr addSquare (Serial 1, Area (Units Map.empty) (Walls Set.empty))


addSquare :: (Point, Square) -> (Serial, Area) -> (Serial, Area)
addSquare (point, square) = case square of
  SquareElf -> addUnit RaceElf point
  SquareGoblin -> addUnit RaceGoblin point
  SquareOpen -> id
  SquareWall -> addWall point


addUnit :: Race -> Point -> (Serial, Area) -> (Serial, Area)
addUnit race point (serial, area) =
  let (newSerial, unit) = makeUnit serial race
  in (newSerial, updateUnits (overUnits (Map.insert point unit)) area)


addWall :: Point -> (serial, Area) -> (serial, Area)
addWall = fmap . updateWalls . overWalls . Set.insert


updateUnits :: (Units -> Units) -> Area -> Area
updateUnits f area = area { areaUnits = f (areaUnits area) }


updateWalls :: (Walls -> Walls) -> Area -> Area
updateWalls f area = area { areaWalls = f (areaWalls area) }


newtype Units = Units
  { unwrapUnits :: Map.Map Point Unit
  } deriving (Show)


overUnits :: (Map.Map Point Unit -> Map.Map Point Unit) -> Units -> Units
overUnits f = Units . f . unwrapUnits


data Unit = Unit
  { unitSerial :: Serial
  , unitRace :: Race
  , unitHealth :: Health
  } deriving (Show)


makeUnit :: Serial -> Race -> (Serial, Unit)
makeUnit serial race = (overSerial (+ 1) serial, Unit serial race (Health 200))


renderUnit :: Unit -> String
renderUnit unit
  = renderRace (unitRace unit)
  <> "-"
  <> renderSerial (unitSerial unit)
  <> "@"
  <> renderHealth (unitHealth unit)


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
  } deriving (Show)


renderHealth :: Health -> String
renderHealth = show . unwrapHealth


newtype Walls = Walls
  { unwrapWalls :: Set.Set Point
  } deriving (Show)


overWalls :: (Set.Set Point -> Set.Set Point) -> Walls -> Walls
overWalls f = Walls . f . unwrapWalls


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