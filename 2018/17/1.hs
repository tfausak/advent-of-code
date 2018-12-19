-- stack --resolver lts-12.0 script
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple
import qualified Text.ParserCombinators.ReadP as Parse
import qualified Text.Read as Read

main = do
  contents <- readFile "input.txt"
  scans <- either fail pure . traverse Read.readEither $ lines contents
  let
    world = apply 1 step $ World
      (Set.singleton (500, 0))
      (Set.unions $ map toPoints scans)
      Set.empty
      Set.empty
  putStr $ render world
  print $ Set.size (rest world) + Set.size (flow world)

render world =
  let ys = clayYs world
  in case (Set.lookupMin ys, Set.lookupMax ys) of
    (Just minY, Just maxY) ->
      unlines $ map (renderRow world) [minY - 1 .. maxY]

renderRow world y =
  let xs = clayXs world
  in case (Set.lookupMin xs, Set.lookupMax xs) of
    (Just minX, Just maxX) ->
      map (renderPoint world y) [minX - 1 .. maxX + 1]

renderPoint world y x = let point = (x, y) in
  if Set.member point $ font world then
    '+'
  else if Set.member point $ clay world then
    '#'
  else if Set.member point $ rest world then
    '~'
  else if Set.member point $ flow world then
    '|'
  else
    '.'

apply n f x = if seq x n < 1 then x else apply (n - 1) f (f x)

step world1 = let
  world2 = world1 { flow = flowAll world1 }
  world3 = world2 { rest = restAll world2 }
  in world3 { flow = Set.difference (flow world3) $ rest world3 }

restAll world
  = foldr (\(y, xs) set -> foldr (\x -> Set.insert (x, y)) set xs) (rest world)
  . filter (rowSupported world)
  . findRows
  $ flow world

findRows = foldr findRow [] . List.sortOn Tuple.swap . Set.toList

findRow (x0, y0) rows = case rows of
  (y1, x1 : xs) : more | y0 == y1 && x0 == x1 - 1 ->
    (y0, x0 : x1 : xs) : more
  _ ->
    (y0, [x0]) : rows

rowSupported world (y, xs) = all (pointSupported world y) xs

pointSupported world y x = let
  down = (x, y + 1)
  left = (x - 1, y)
  right = (x + 1, y)
  onBottom = isOccupied world down
  onLeft = isOccupied world left || Set.member left (flow world)
  onRight = isOccupied world right || Set.member right (flow world)
  in onBottom && onLeft && onRight

flowAll world
  = Set.unions
  . map (flowOne world)
  . Set.toList
  . Set.union (font world)
  $ flow world

flowOne world point@(x, y) = let
  down = (x, y + 1)
  left = (x - 1, y)
  right = (x + 1, y)
  in if isOccupied world down then
    Set.difference (Set.fromList [left, point, right]) $ clay world
  else if Just (snd down) > Set.lookupMax (clayYs world) then
    Set.empty
  else
    Set.singleton down

clayXs = Set.map fst . clay

clayYs = Set.map snd . clay

isOccupied world point
  = Set.member point (clay world)
  || Set.member point (rest world)

data World
  = World { font, clay, rest, flow :: Set.Set (Int, Int) }
  deriving Show

toPoints (Scan axis (Vein x y1 y2)) =
  Set.fromList $ map (orient axis x) [y1 .. y2]

orient axis x y = case axis of
  Horizontal -> (y, x)
  Vertical -> (x, y)

data Scan
  = Scan Axis Vein
  deriving Show

instance Read Scan where
  readsPrec _ = Parse.readP_to_S $ Parse.choice
    [ Scan Horizontal <$> parseVein 'y' 'x'
    , Scan Vertical <$> parseVein 'x' 'y'
    ]

data Axis
  = Horizontal
  | Vertical
  deriving Show

data Vein
  = Vein Int Int Int
  deriving Show

parseVein x y = Vein
  <$> (Parse.char x *> Parse.char '=' *> parseInt)
  <*> (Parse.string ", " *> Parse.char y *> Parse.char '=' *> parseInt)
  <*> (Parse.string ".." *> parseInt)

parseInt = either fail pure . Read.readEither =<< Parse.munch1 Char.isDigit
