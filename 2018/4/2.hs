-- stack --resolver lts-12.0 script
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Text.ParserCombinators.ReadP as Parse

main
  = print
  . (\ ( g, ( m, _ ) ) -> g * m)
  . List.maximumBy (Ord.comparing (snd . snd))
  . Map.toList
  . fmap identify
  . Map.fromListWith (++)
  . (\ ( _, _, xs ) -> xs)
  . foldl combine ( Nothing, Nothing, [] )
  . List.sortOn time
  . map read
  . lines
  =<< getContents

identify
  = List.maximumBy (Ord.comparing snd)
  . Map.toList
  . Map.fromListWith (+)
  . flip zip (repeat 1)
  . concatMap (\ ( l, h ) -> [ l .. h - 1 ])

combine ( mg, mt, xs ) x =
  case ( action x, mg, mt ) of
    ( Start g, _, _ ) ->
      ( Just g, Nothing, xs )
    ( Sleep, Just g, Nothing ) ->
      ( Just g, Just $ time x, xs )
    ( Wake, Just g, Just t ) ->
      ( Just g, Nothing, ( g, [ ( minute t, minute $ time x ) ] ) : xs )

data Record = Record { time :: Time, action :: Action }

data Time = Time { year, month, day, hour, minute :: Int } deriving (Eq, Ord)

data Action = Start Int | Sleep | Wake

instance Read Record where
  readsPrec _ = Parse.readP_to_S $ Record
    <$> (Time
      <$> (Parse.char '[' *> number)
      <*> (Parse.char '-' *> number)
      <*> (Parse.char '-' *> number)
      <*> (Parse.char ' ' *> number)
      <*> (Parse.char ':' *> number))
    <*> (Parse.string "] " *> Parse.choice
      [ Start <$> (Parse.string "Guard #" *> number <* Parse.string " begins shift")
      , Sleep <$ Parse.string "falls asleep"
      , Wake <$ Parse.string "wakes up"
      ])

number = Parse.readS_to_P reads
