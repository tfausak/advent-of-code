-- stack --resolver ghc-8.6.3 script
main
  = print
  . (\ x -> x :: Int)
  . sum
  . map
    ( uncurry (-)
    . foldr
      (\ x (hi, lo) -> (max hi x, min lo x))
      (minBound, maxBound)
    . map read
    . words
    )
  . lines
  =<< readFile "input.txt"
