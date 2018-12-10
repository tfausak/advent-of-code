-- stack --resolver lts-12.0 script
import qualified Text.ParserCombinators.ReadP as Parse
import qualified Text.Printf as Printf

main = do
  lights <- map read . lines <$> getContents
  flip mapM_ [ 10000 .. 10030 ] $ \ n -> let
    ls = map (move n) lights
    minX = minimum $ map (\ (Light x _ _ _) -> x) ls
    maxX = maximum $ map (\ (Light x _ _ _) -> x) ls
    minY = minimum $ map (\ (Light _ y _ _) -> y) ls
    maxY = maximum $ map (\ (Light _ y _ _) -> y) ls
    file = Printf.printf "/tmp/advent-of-code/day-10-part-1-%02d.svg" n
    contents = Printf.printf
      "<svg xmlns='http://www.w3.org/2000/svg' viewBox='%d %d %d %d'>%s</svg>"
      minX minY maxX maxY
      (concatMap render ls :: String)
    in writeFile file contents

render (Light x y _ _) = Printf.printf "<circle r='1' cx='%d' cy='%d' />" x y

move n (Light x y vx vy) = Light (x + n * vx) (y + n * vy) vx vy

data Light = Light Int Int Int Int

instance Eq Light where
  Light x1 y1 vx1 vy1 == Light x2 y2 vx2 vy2 =
    x1 == x2 && y1 == y2 && vx1 == vx2 && vy1 == vy2

instance Ord Light where
  compare (Light x1 y1 vx1 vy1) (Light x2 y2 vx2 vy2) =
    compare x1 x2 <>
    compare y1 y2 <>
    compare vx1 vx2 <>
    compare vy1 vy2

instance Show Light where
  show (Light x y _ _) = show ( x, y )

instance Read Light where
  readsPrec _ = Parse.readP_to_S $ Light
    <$> (Parse.string "position=<" *> Parse.skipSpaces *> number)
    <*> (Parse.char ',' *> Parse.skipSpaces *> number)
    <*> (Parse.string "> velocity=<" *> Parse.skipSpaces *> number)
    <*> (Parse.char ',' *> Parse.skipSpaces *> number <* Parse.char '>')

number = pure . read =<< Parse.munch1 (\ c -> c == '-' || '0' <= c && c <= '9')
