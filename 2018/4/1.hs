-- stack --resolver lts-12.0 script
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Text.ParserCombinators.ReadP as Parse

main
  = mapM_ print
  -- TODO: Reduce this information into something useful, like "guard X fell
  -- asleep at Y and woke up at Z."
  . List.sortOn time
  . map read
  . lines
  =<< getContents

data Record = Record { time :: Time, event :: Event } deriving Show

data Time = Time { year, month, day, hour, minute :: Word } deriving (Eq, Ord, Show)

data Event = BeganShift Word | FellAsleep | WokeUp deriving Show

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
