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
  print . maximum $ map (\ p -> ( squarePower cells p, p )) points

squarePower cells ( x, y ) = let
  ( ( x1, y1 ), ( x2, y2 ) ) = Array.bounds cells
  in if x < x1 || x + 2 > x2 || y < y1 || y + 2 > y2 then Nothing else Just $
  cells Array.! ( x + 0, y + 0 ) +
  cells Array.! ( x + 1, y + 0 ) +
  cells Array.! ( x + 2, y + 0 ) +
  cells Array.! ( x + 0, y + 1 ) +
  cells Array.! ( x + 1, y + 1 ) +
  cells Array.! ( x + 2, y + 1 ) +
  cells Array.! ( x + 0, y + 2 ) +
  cells Array.! ( x + 1, y + 2 ) +
  cells Array.! ( x + 2, y + 2 )

cellPower n ( x, y ) = (mod (div ((((x + 10) * y) + n) * (x + 10)) 100) 10) - 5
