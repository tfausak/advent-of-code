-- stack --resolver lts-12.0 script
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

main = do
  points <- pure
    . zip [ 0 .. ]
    . fmap (tuple . fmap read . words)
    . lines
    . filter (/= ',')
    =<< getContents
  let ( xs, ys ) = unzip $ fmap snd points
  print
    . List.maximum
    . Map.elems
    . Map.fromListWith (+)
    . flip zip (repeat 1)
    $ concatMap
      (\ x -> Maybe.mapMaybe
        (\ y -> closest points ( x, y ))
        [ minimum ys .. maximum ys ])
      [ minimum xs .. maximum xs ]

tuple [ x, y ] = ( x, y )

closest ps p1 =
  case List.sort $ fmap (\ ( i, p2 ) -> ( distance p1 p2, i )) ps of
    ( d1, i1 ) : ( d2, i2 ) : _ | d1 /= d2 -> Just i1
    _ -> Nothing

distance ( x1, y1 ) ( x2, y2 ) = abs (x1 - x2) + abs (y1 - y2)
