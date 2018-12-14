-- stack --resolver lts-12.0 script
import qualified Control.Monad as Monad
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Either as Either
import qualified Data.IORef as IORef
import qualified Data.Map as Map
import qualified System.Exit as Exit

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let
    (tracks, carts) = Bifunctor.bimap Map.fromList Map.fromList
      (Either.partitionEithers (concatMap
        (\ (y, row) -> concatMap
          (\ (x, square) -> parseAt (Point y x) square)
          (zip (map X [0 ..]) row))
        (zip (map Y [0 ..]) (lines contents))))
  cartsRef <- IORef.newIORef carts
  Monad.forever (simulate tracks cartsRef)

parseAt :: Point -> Char -> [Either (Point, Track) (Point, Cart)]
parseAt point square = case square of
  '|' -> [Left (point, TrackVertical)]
  '-' -> [Left (point, TrackHorizontal)]
  '/' -> [Left (point, TrackCurveJ)]
  '\\' -> [Left (point, TrackCurveL)]
  '+' -> [Left (point, TrackIntersection)]
  '^' -> [Left (point, TrackVertical), Right (point, makeCart DirectionUp)]
  '>' -> [Left (point, TrackHorizontal), Right (point, makeCart DirectionRight)]
  'v' -> [Left (point, TrackVertical), Right (point, makeCart DirectionDown)]
  '<' -> [Left (point, TrackHorizontal), Right (point, makeCart DirectionLeft)]
  _ -> []

simulate :: Tracks -> IORef.IORef Carts -> IO ()
simulate tracks cartsRef = do
  carts <- IORef.readIORef cartsRef
  mapM_ (step tracks cartsRef) (Map.keys carts)

step :: Tracks -> IORef.IORef Carts -> Point -> IO ()
step tracks cartsRef point = do
  carts <- IORef.readIORef cartsRef
  case Map.keys carts of
    [lastPoint] -> do
      putStrLn ("Last cart at " <> show lastPoint)
      Exit.exitSuccess
    _ -> case Map.lookup point carts of
      Nothing -> putStrLn ("Cart at " <> show point <> " no longer exists.")
      Just cart -> do
        IORef.modifyIORef cartsRef (Map.delete point)
        let newPoint = move point (cartDirection cart)
        if Map.member newPoint carts
          then do
            putStrLn ("Cart at " <> show point <> " crashed into cart at " <> show newPoint <> ".")
            IORef.modifyIORef cartsRef (Map.delete newPoint)
          else do
            let
              newCart = case Map.lookup newPoint tracks of
                Just TrackCurveJ -> curveJ cart
                Just TrackCurveL -> curveL cart
                Just TrackIntersection -> intersect cart
                _ -> cart
            IORef.modifyIORef cartsRef (Map.insert newPoint newCart)

curveJ :: Cart -> Cart
curveJ cart = cart
  { cartDirection = case cartDirection cart of
    DirectionUp -> DirectionRight
    DirectionRight -> DirectionUp
    DirectionDown -> DirectionLeft
    DirectionLeft -> DirectionDown
  }

curveL :: Cart -> Cart
curveL cart = cart
  { cartDirection = case cartDirection cart of
    DirectionUp -> DirectionLeft
    DirectionRight -> DirectionDown
    DirectionDown -> DirectionRight
    DirectionLeft -> DirectionUp
  }

intersect :: Cart -> Cart
intersect cart = cart
  { cartChoice = nextChoice (cartChoice cart)
  , cartDirection = case cartChoice cart of
    TurnLeft -> turnLeft (cartDirection cart)
    GoStraight -> cartDirection cart
    TurnRight -> turnRight (cartDirection cart)
  }

turnLeft :: Direction -> Direction
turnLeft direction = case direction of
  DirectionUp -> DirectionLeft
  DirectionLeft -> DirectionDown
  DirectionDown -> DirectionRight
  DirectionRight -> DirectionUp

turnRight :: Direction -> Direction
turnRight direction = case direction of
  DirectionUp -> DirectionRight
  DirectionRight -> DirectionDown
  DirectionDown -> DirectionLeft
  DirectionLeft -> DirectionUp

nextChoice :: Choice -> Choice
nextChoice choice = case choice of
  TurnLeft -> GoStraight
  GoStraight -> TurnRight
  TurnRight -> TurnLeft

move :: Point -> Direction -> Point
move point direction = case direction of
  DirectionUp -> point { pointY = Y (yToInt (pointY point) - 1) }
  DirectionRight -> point { pointX = X (xToInt (pointX point) + 1) }
  DirectionDown -> point { pointY = Y (yToInt (pointY point) + 1) }
  DirectionLeft -> point { pointX = X (xToInt (pointX point) - 1) }

type Tracks = Map.Map Point Track

type Carts = Map.Map Point Cart

data Point = Point
  { pointY :: Y
  , pointX :: X
  } deriving (Eq, Ord)

instance Show Point where
  show point = show (pointX point, pointY point)

newtype X = X
  { xToInt :: Int
  } deriving (Eq, Ord)

instance Show X where
  show = show . xToInt

newtype Y = Y
  { yToInt :: Int
  } deriving (Eq, Ord)

instance Show Y where
  show = show . yToInt

data Track
  = TrackVertical
  | TrackHorizontal
  | TrackCurveJ
  | TrackCurveL
  | TrackIntersection
  deriving (Show)

data Cart = Cart
  { cartChoice :: Choice
  , cartDirection :: Direction
  } deriving Show

makeCart :: Direction -> Cart
makeCart = Cart TurnLeft

data Choice
  = TurnLeft
  | GoStraight
  | TurnRight
  deriving Show

data Direction
  = DirectionUp
  | DirectionRight
  | DirectionDown
  | DirectionLeft
  deriving Show
