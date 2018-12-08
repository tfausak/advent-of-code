-- stack --resolver lts-12.0 script

main = do
  points <- map (tuple . map read . words)
    . lines
    . filter (/= ',')
    <$> getContents
  let ( xs, ys ) = unzip points
  print
    . length
    . filter (\ p -> sum (map (distance p) points) < 10000)
    $ concatMap
      (\ x -> map
        (\ y -> ( x, y ))
        [ minimum ys .. maximum ys ])
      [ minimum xs .. maximum xs ]

tuple [ x, y ] = ( x, y )

distance ( x1, y1 ) ( x2, y2 ) = abs (x1 - x2) + abs (y1 - y2)
