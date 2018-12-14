-- stack --resolver lts-12.0 script
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Tuple as Tuple

main = do
  contents <- getContents
  let
    (tracks, carts) = Bifunctor.bimap Map.fromList Map.fromList
      (Either.partitionEithers (concatMap
        (\ (y, line) -> concatMap
          (\ (x, char) -> case char of
            '/' -> [Left ((y, x), J)]
            '\\' -> [Left ((y, x), L)]
            '+' -> [Left ((y, x), X)]
            '-' -> [Left ((y, x), H)]
            '|' -> [Left ((y, x), V)]
            '^' -> [Left ((y, x), V), Right ((y, x), Cart LL N)]
            '>' -> [Left ((y, x), H), Right ((y, x), Cart LL E)]
            'v' -> [Left ((y, x), V), Right ((y, x), Cart LL S)]
            '<' -> [Left ((y, x), H), Right ((y, x), Cart LL W)]
            ' ' -> [])
          (indexed line))
        (indexed (lines contents))))
  putStrLn . debug tracks . apply (step tracks) 150 $ carts

apply f n x = if n < 1 then x else apply f (n - 1) (f x)

debug tracks carts = let
  (ys1, xs1) = unzip (Map.keys tracks)
  (ys2, xs2) = unzip (Map.keys carts)
  (xs, ys) = (xs1 <> xs2, ys1 <> ys2)
  (minX, maxX) = (minimum xs, maximum xs)
  (minY, maxY) = (minimum ys, maximum ys)
  in unlines (map
    (\ y -> map
      (\ x -> let p = (y, x) in case Map.lookup p carts of
        Just cart -> case cart of
          Cart _ direction -> case direction of
            N -> '^'
            E -> '>'
            S -> 'v'
            W -> '<'
        Nothing -> case Map.lookup p tracks of
          Just track -> case track of
            J -> '/'
            L -> '\\'
            X -> '+'
            H -> '-'
            V -> '|'
          Nothing -> ' ')
      [minX .. maxX])
    [minY .. maxY])

step tracks carts
  = Map.fromListWithKey (\ (y, x) _ _ -> error $ "crash at " <> show (x, y))
  . map (\ ((y, x), cart) -> case cart of
    Cart turn direction -> let
      p = case direction of
        N -> (y - 1, x)
        E -> (y, x + 1)
        S -> (y + 1, x)
        W -> (y, x - 1)
      in case Map.lookup p carts of
        Just _ -> error $ "crash at " <> show (Tuple.swap p)
        Nothing -> case Map.lookup p tracks of
          Nothing -> (p, cart)
          Just track -> case track of
            J -> (p, jCurve cart)
            L -> (p, lCurve cart)
            X -> (p, intersect cart)
            H -> (p, cart)
            V -> (p, cart))
  . Map.toAscList
  $ carts

jCurve (Cart turn direction) = Cart turn (case direction of
  N -> E
  E -> N
  S -> W
  W -> S)

lCurve (Cart turn direction) = Cart turn (case direction of
  N -> W
  E -> S
  S -> E
  W -> N)

intersect (Cart turn direction) = Cart
  (case turn of
    LL -> SS
    SS -> RR
    RR -> LL)
  (case turn of
    LL -> case direction of
      N -> W
      E -> N
      S -> E
      W -> S
    SS -> direction
    RR -> case direction of
      N -> E
      E -> S
      S -> W
      W -> N)

indexed = zip [ 0 .. ]

data Track = J | L | X | H | V deriving Show

data Cart = Cart Turn Direction deriving Show

data Turn = LL | SS | RR deriving Show

data Direction = N | E | S | W deriving Show
