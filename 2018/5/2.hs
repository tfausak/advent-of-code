-- stack --resolver lts-12.0 script
import qualified Data.Char as Char
import qualified Data.List as List

main = do
  polymer <- filter Char.isAlpha <$> getContents
  print . List.minimum $ map (reactWith polymer) [ 'a' .. 'z' ]

reactWith polymer unit =
  length . foldr react [] $ filter (shouldRemove unit) polymer

react c1 s1 =
  case s1 of
    [] -> [ c1 ]
    c2 : s2 -> if shouldReact c1 c2
      then s2
      else c1 : s1

shouldRemove x y =
  x /= Char.toLower y

shouldReact x y =
  x /= y && Char.toLower x == Char.toLower y
