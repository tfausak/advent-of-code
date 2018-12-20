-- stack --resolver lts-12.0 script
import qualified Data.Graph.Inductive as Graph
import qualified Data.Map as Map
import qualified Debug.Trace as Debug
import qualified Text.Printf as Printf

main
  = print
  . maximum
  . map snd
  . Graph.level 0
  . walk [] origin (Map.singleton (0, 0) 0) []
  =<< readFile "input.txt"

origin = (0, 0)

walk
  :: [Point]
  -> Point
  -> Map.Map (Int, Int) Int
  -> [Graph.UEdge]
  -> String
  -> Graph.UGr
walk ps p ns es ds = case (ds, ps) of
  ("", _) -> Graph.mkGraph (map (\ n -> (n, ())) $ Map.elems ns) es
  ('^' : ds', _) -> walk ps p ns es ds'
  ('N' : ds', _) -> let q = goN p; ns' = Map.insertWith (flip const) q (Map.size ns) ns in walk ps q ns' (edge ns' p q : es) ds'
  ('E' : ds', _) -> let q = goE p; ns' = Map.insertWith (flip const) q (Map.size ns) ns in walk ps q ns' (edge ns' p q : es) ds'
  ('S' : ds', _) -> let q = goS p; ns' = Map.insertWith (flip const) q (Map.size ns) ns in walk ps q ns' (edge ns' p q : es) ds'
  ('W' : ds', _) -> let q = goW p; ns' = Map.insertWith (flip const) q (Map.size ns) ns in walk ps q ns' (edge ns' p q : es) ds'
  ('(' : ds', _) -> walk (p : ps) p ns es ds'
  ('|' : ds', q : _) -> walk ps q ns es ds'
  (')' : ds', q : qs) -> walk qs q ns es ds'
  ('$' : ds', _) -> walk ps p ns es ds'
  ('\n' : ds', _) -> walk ps p ns es ds'

goN (x, y) = (x, y - 1)
goE (x, y) = (x + 1, y)
goS (x, y) = (x, y + 1)
goW (x, y) = (x - 1, y)

node ns p = (ns Map.! p, ())

edge ns p q = (ns Map.! p, ns Map.! q, ())

type Point = (Int, Int)
