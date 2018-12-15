-- stack --resolver lts-12.0 script
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set


main :: IO ()
main = do
  input <- readFile "input.txt"
  area <- either fail pure $ parseArea input
  putStrLn . Maybe.fromJust $ renderArea area


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


newtype Serial = Serial
  { unwrapSerial :: Int
  } deriving (Show)


overSerial :: (Int -> Int) -> Serial -> Serial
overSerial f = Serial . f . unwrapSerial


data Race
  = RaceElf
  | RaceGoblin
  deriving (Show)


newtype Health = Health
  { unwrapHealth :: Int
  } deriving (Show)


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


pointed :: (a -> Either String b) -> (Point, a) -> Either String (Point, b)
pointed parse (point, input) = case parse input of
  Left message -> Left ("at " <> show point <> ": " <> message)
  Right output -> Right (point, output)


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


withXs :: [a] -> [(X, a)]
withXs = zip (map X [0 ..])


newtype Y = Y
  { unwrapY :: Int
  } deriving (Eq, Ord, Show)


instance Enum Y where
  fromEnum = unwrapY
  toEnum = Y


withYs :: [a] -> [(Y, a)]
withYs = zip (map Y [0 ..])
