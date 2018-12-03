-- stack --resolver lts-12.0 script
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Text.ParserCombinators.ReadP as Parse
import qualified Text.Read as Read

main
  = print
  . Map.size
  . Map.filter (> 1)
  . foldr (\ ( x, y ) -> Map.insertWith (+) ( x, y ) 1) Map.empty
  . concatMap points
  . Maybe.mapMaybe Read.readMaybe
  . lines
  =<< getContents

data Claim
  = Claim Word Word Word Word Word
  deriving Show

instance Read Claim where
  readsPrec _ = Parse.readP_to_S $ Claim
    <$> (Parse.string "#" *> number)
    <*> (Parse.string " @ " *> number)
    <*> (Parse.string "," *> number)
    <*> (Parse.string ": " *> number)
    <*> (Parse.string "x" *> number)

number
  = maybe (fail "not a number") pure
  . Read.readMaybe
  =<< Parse.munch1 Char.isDigit

points (Claim _ x y w h) = (,)
  <$> [ x .. x + w - 1 ]
  <*> [ y .. y + h - 1 ]
