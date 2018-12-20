-- stack --resolver lts-12.0 script
import qualified Data.Graph.Inductive as Graph
import qualified Data.Map as Map

main
  = print
  . length
  . filter (>= 1000)
  . map snd
  . Graph.level 0
  . walk [] origin (Map.singleton origin 0) []
  =<< readFile "input.txt"

origin = (0, 0)

walk ps p ns es ds = case (ds, ps) of
  ('^' : ds', _) -> walk ps p ns es ds'
  ('(' : ds', _) -> walk (p : ps) p ns es ds'
  ('|' : ds', q : _) -> walk ps q ns es ds'
  (')' : ds', q : qs) -> walk qs q ns es ds'
  ('$' : _, _) -> Graph.mkGraph (mkNodes ns) es :: Graph.UGr
  (d : ds', _) -> step p d ns es ps ds'

step source@(x, y) direction nodes edges stack directions = let
  destination = case direction of
    'N' -> (x, y - 1)
    'E' -> (x + 1, y)
    'S' -> (x, y + 1)
    'W' -> (x - 1, y)
  newNodes = Map.insertWith (\_ old -> old) destination (Map.size nodes) nodes
  newEdges = mkEdge newNodes source destination : edges
  in walk stack destination newNodes newEdges directions

mkNodes = map (\ node -> (node, ())) . Map.elems

mkEdge nodes source destination =
  (nodes Map.! source, nodes Map.! destination, ())
