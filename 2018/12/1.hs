-- stack --resolver lts-12.0 script
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.ParserCombinators.ReadP  as Parse

main = do
  first : _ : rest <- lines <$> getContents
  let m = Map.fromList $ map (parse note) rest
  print . sum . Set.toList . apply (solve m) 20 $ parse row first

apply f n x = if n < 1 then x else apply f (n - 1) (f x)

solve m x = Set.fromDistinctAscList $ filter (step m x)
  [Set.findMin x - 2 .. Set.findMax x + 2]

step m x i = Map.findWithDefault False
  ( Set.member (i - 2) x
  , Set.member (i - 1) x
  , Set.member  i      x
  , Set.member (i + 1) x
  , Set.member (i + 2) x
  ) m

parse p = head . map fst . filter (null . snd) . Parse.readP_to_S p

note = (,)
  <$> ((,,,,) <$> pot <*> pot <*> pot <*> pot <*> pot)
  <*> (Parse.string " => " *> pot)

row = Set.fromDistinctAscList . map fst . filter snd . zip [0 ..]
  <$> (Parse.string "initial state: " *> Parse.many pot)

pot = (False <$ Parse.char '.') Parse.<++ (True <$ Parse.char '#')
