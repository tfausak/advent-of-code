-- stack --resolver lts-12.0 script
import qualified Data.Array as Array

main = do
  serial <- read <$> getContents
  let
    ( minX, maxX ) = ( 1, 300 )
    ( minY, maxY ) = ( 1, 300 )
    points = concatMap
      (\ x -> map
        (\ y -> ( x, y ))
        [ minY .. maxY ])
      [ minX .. maxX ]
    cells = Array.array
      ( ( minX, minY ), ( maxX, maxY ) )
      (map (\ p -> ( p, cellPower serial p )) points)
  -- TODO: This is too slow.
  print . maximum $ concatMap
    (\ size -> map
      (\ point -> ( squarePower cells size point, point, size ))
      points)
    [ 1 .. 300 ]

squarePower cells size ( left, top ) =
  let ( ( x1, y1 ), ( x2, y2 ) ) = Array.bounds cells
  in if left < x1 || left + size >= x2 || top < y1 || top + size >= y2
  then Nothing
  else Just . sum $ concatMap
    (\ x -> map
      (\ y -> cells Array.! ( x, y ))
      [ top .. top + size - 1])
    [ left .. left + size - 1 ]

cellPower n ( x, y ) = (mod (div ((((x + 10) * y) + n) * (x + 10)) 100) 10) - 5
