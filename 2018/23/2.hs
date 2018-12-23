-- stack --resolver lts-12.25 script
import qualified Data.SBV as SBV
import qualified Text.ParserCombinators.ReadP as Parse

main = do
  nanobots <- map read . lines <$> readFile "input.txt"
  model <- SBV.optimize SBV.Lexicographic $ do
    [x, y, z] <- SBV.sIntegers ["x", "y", "z"]
    SBV.maximize "nanobots-in-range" . sum $ map
      ((\ n -> n :: SBV.SInteger) . inRange x y z) nanobots
    SBV.minimize "distance-to-origin" $ manhattanDistance 0 0 0 x y z
  print model

inRange x y z n = SBV.oneIf . (SBV..<= SBV.literal (nr n)) $ manhattanDistance
  (SBV.literal $ nx n) (SBV.literal $ ny n) (SBV.literal $ nz n)
  x y z

absoluteValue n = SBV.ite (n SBV..< 0) (negate n) n

manhattanDistance x0 y0 z0 x1 y1 z1 =
  absoluteValue (x0 - x1) + absoluteValue (y0 - y1) + absoluteValue (z0 - z1)

data Nanobot = Nanobot { nx, ny, nz, nr :: Integer } deriving Show

instance Read Nanobot where
  readsPrec n = let parseInt = Parse.readS_to_P (readsPrec n) in
    Parse.readP_to_S (Nanobot
      <$> (Parse.string "pos=<" *> parseInt)
      <*> (Parse.char ',' *> parseInt)
      <*> (Parse.char ',' *> parseInt)
      <*> (Parse.string ">, r=" *> parseInt))
