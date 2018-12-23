-- stack --resolver lts-12.0 script
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Text.ParserCombinators.ReadP as Parse

main = do
  nanobots <- map read . lines <$> readFile "input.txt"
  let strongest = List.maximumBy (Ord.comparing r) nanobots
  print . length $ filter (inRange strongest) nanobots

inRange a b = distance a b <= r a

distance a b = abs (x a - x b) + abs (y a - y b) + abs (z a - z b)

data Nanobot = Nanobot { x, y, z, r :: Int } deriving (Show)

instance Read Nanobot where
  readsPrec n = Parse.readP_to_S (Nanobot
    <$> (Parse.string "pos=<" *> Parse.readS_to_P (readsPrec n))
    <*> (Parse.char ',' *> Parse.readS_to_P (readsPrec n))
    <*> (Parse.char ',' *> Parse.readS_to_P (readsPrec n))
    <*> (Parse.string ">, r=" *> Parse.readS_to_P (readsPrec n)))
