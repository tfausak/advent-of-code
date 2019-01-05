-- stack --resolver ghc-8.6.3 script
main
  = print
  . sum
  . map (solve . map read . words)
  . lines
  =<< readFile "input.txt"

solve xs = head $ concatMap (\ x -> concatMap (divisor x) xs) xs

divisor x y = let (q, r) = quotRem x y in if q /= 1 && r == 0 then [q] else []
