-- stack --resolver lts-12.0 script
import qualified Data.Char as Char
import qualified Text.ParserCombinators.ReadP as Parse

main
  = print
  . n
  . fst
  . head
  . filter (uncurry (all . disjoint))
  . sublists
  . map read
  . lines
  =<< getContents

sublists l = map (\ e -> ( e, filter (/= e) l )) l

disjoint a b =
  x b >= x a + w a || x a >= x b + w b ||
  y b >= y a + h a || y a >= y b + h b

data Claim = Claim { n, x, y, w, h :: Word } deriving Eq

instance Read Claim where
  readsPrec _ = Parse.readP_to_S $ Claim
    <$> (Parse.string "#" *> number)
    <*> (Parse.string " @ " *> number)
    <*> (Parse.string "," *> number)
    <*> (Parse.string ": " *> number)
    <*> (Parse.string "x" *> number)

number = pure . read =<< Parse.munch1 Char.isDigit
