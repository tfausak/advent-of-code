-- stack --resolver lts-12.0 script
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Text.ParserCombinators.ReadP as Parse

main
  = print
  . (\ (g, m) -> g * m)
  . (\ (g, xs) -> (g, todo xs))
  . List.maximumBy (Ord.comparing (\(_, xs) -> sum (map (\(_, _, d) -> d) xs)))
  . Map.toList
  . Map.fromListWith (<>)
  . map (\ (g, s, e, d) -> (g, [(s, e, d)]))
  . (\ (_, _, x) -> x)
  . foldl
    (\ (maybeGuard, maybeTime, result) record -> case action record of
      BeganShift guard -> (Just guard, Nothing, result)
      FellAsleep -> (maybeGuard, Just (time record), result)
      WokeUp -> case (maybeGuard, maybeTime) of
        (Just guard, Just t) ->
          ( Just guard
          , Nothing
          , ( guard
            , minute t
            , minute (time record)
            , minute (time record) - minute t
            ) : result
          ))
    (Nothing, Nothing, [])
  . List.sortOn time
  . map read
  . lines
  =<< getContents

todo
  = fst
  . List.maximumBy (Ord.comparing snd)
  . Map.toList
  . Map.fromListWith (+)
  . flip zip (repeat 1)
  . concatMap (\ (s, e, _) -> [ s .. e - 1 ])

data Record = Record { time :: Time, action :: Action } deriving Show

data Time = Time { year, month, day, hour, minute :: Word } deriving (Eq, Ord, Show)

data Action = BeganShift Word | FellAsleep | WokeUp deriving Show

instance Read Record where
  readsPrec n = Parse.readP_to_S $ Record
    <$> (Time
      <$> (Parse.char '[' *> number)
      <*> (Parse.char '-' *> number)
      <*> (Parse.char '-' *> number)
      <*> (Parse.char ' ' *> number)
      <*> (Parse.char ':' *> number))
    <*> (Parse.string "] " *> Parse.choice
      [ BeganShift <$> (Parse.string "Guard #" *> number <* Parse.string " begins shift")
      , FellAsleep <$ Parse.string "falls asleep"
      , WokeUp <$ Parse.string "wakes up"
      ])

number = pure . read =<< Parse.munch1 Char.isDigit
