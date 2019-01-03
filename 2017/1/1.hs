-- stack --resolver ghc-8.6.3 script
import qualified Data.Maybe as Maybe

main
  = print
  . sum
  . map fst
  . filter (uncurry (==))
  . zipDrop 1
  . Maybe.mapMaybe toDigit
  =<< readFile "input.txt"

zipDrop n xs = zip xs . drop n $ cycle xs

toDigit x = if isDigit x then Just $ fromEnum x - 48 else Nothing

isDigit x = '0' <= x && x <= '9'
