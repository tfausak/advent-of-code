-- stack --resolver lts-12.0 script
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

main
  = print
  . maximum
  . Map.elems
  . fst
  . solve
  . map read
  . words
  . filter (\ c -> c == ' ' || '0' <= c && c <= '9')
  =<< getContents

solve [ playerCount, lastMarble ] = let limit = lastMarble * 100 in foldr
  play
  ( Map.fromList $ zip [ 0 .. playerCount - 1 ] (repeat 0)
  , Seq.singleton 0
  )
  [ limit, limit - 1 .. 1 ]

play marble ( scores, circle ) =
  if mod marble 23 == 0
  then let Just ( m, c ) = unsnoc $ rotate 7 circle in
    ( Map.insertWith (+) (mod marble $ Map.size scores) (marble + m) scores
    , rotate (-1) c
    )
  else ( scores, snoc marble $ rotate (-1) circle )

rotate n deque
  | n > 0 = rotate (n - 1) $ shiftRight deque
  | n < 0 = rotate (n + 1) $ shiftLeft deque
  | otherwise = deque

shiftLeft q = maybe q (uncurry snoc) $ uncons q

shiftRight q = maybe q (uncurry cons) $ unsnoc q

cons x q = x Seq.<| q

uncons q = case Seq.viewl q of
  Seq.EmptyL -> Nothing
  h Seq.:< t -> Just ( h, t )

snoc x q = q Seq.|> x

unsnoc q = case Seq.viewr q of
  Seq.EmptyR -> Nothing
  t Seq.:> h -> Just ( h, t )
