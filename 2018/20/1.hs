-- stack --resolver lts-12.0 script
import qualified Data.Graph.Inductive as Graph
import qualified Data.Map as Map
import qualified Debug.Trace as Debug

main
  = putStrLn
  . render
  . Map.insert origin 'X'
  . walk [] origin Map.empty
  =<< readFile "input.txt"

render area = let
  ((xl, xh), (yl, yh)) = foldr
    (\ (x, y) ((xl, xh), (yl, yh)) ->
      ((min xl x, max xh x), (min yl y, max yh y)))
    ((maxBound, minBound), (maxBound, minBound))
    (Map.keys area)
  in unlines (map
    (\ y -> map
      (\ x -> Map.findWithDefault '#' (x, y) area)
      [xl .. xh])
    [yl .. yh])

origin :: (Int, Int)
origin = (0, 0)

walk points (x, y) area directions = case directions of
  "" -> area
  d : ds -> case d of
    '^' -> walk points (x, y) area ds
    'W' -> walk points (x - 2, y) (horizontal (x - 1, y) area) ds
    'E' -> walk points (x + 2, y) (horizontal (x + 1, y) area) ds
    'N' -> walk points (x, y - 2) (vertical (x, y - 1) area) ds
    'S' -> walk points (x, y + 2) (vertical (x, y + 1) area) ds
    '(' -> walk ((x, y) : points) (x, y) area ds
    '|' -> case points of { p : ps -> walk points p area ds }
    ')' -> case points of { p : ps -> walk ps p area ds }
    '$' -> walk points (x, y) area ds
    '\n' -> walk points (x, y) area ds
    _ -> error (show d)

horizontal (x, y) area
  = Map.insert (x - 1, y) '.'
  . Map.insert (x + 1, y) '.'
  . Map.insert (x, y - 1) '#'
  . Map.insert (x, y + 1) '#'
  . Map.insert (x, y) '|'
  $ area

vertical (x, y) area
  = Map.insert (x, y - 1) '.'
  . Map.insert (x, y + 1) '.'
  . Map.insert (x - 1, y) '#'
  . Map.insert (x + 1, y) '#'
  . Map.insert (x, y) '-'
  $ area
