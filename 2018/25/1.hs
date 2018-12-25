-- stack --resolver lts-13.0 script
import qualified Control.Monad as Monad
import qualified Data.IORef as IORef
import qualified Data.Set as Set
import qualified Data.UnionFind.IO as Union
import qualified Text.ParserCombinators.ReadP as Parse

main = do
  xs <- mapM toPoint . map read . lines =<< readFile "input.txt"

  Monad.forM_ xs $ \ (l1, p1) ->
    Monad.forM_ xs $ \ (l2, p2) ->
      Monad.when (distance l1 l2 <= 3) $
        Union.union p1 p2

  ref <- IORef.newIORef Set.empty
  Monad.forM_ xs $ \ (_, p) ->
    IORef.modifyIORef ref . Set.insert =<< Union.descriptor p

  print . Set.size =<< IORef.readIORef ref

distance p q
  = abs (x p - x q)
  + abs (y p - y q)
  + abs (z p - z q)
  + abs (t p - t q)

toPoint l = (,)
  <$> pure l
  <*> Union.fresh l

data Location = Location { x, y, z, t :: Int } deriving (Eq, Ord, Show)

instance Read Location where
  readsPrec n =
    let parseInt = Parse.readS_to_P $ readsPrec n
    in Parse.readP_to_S (Location
      <$> parseInt
      <*> (Parse.char ',' *> parseInt)
      <*> (Parse.char ',' *> parseInt)
      <*> (Parse.char ',' *> parseInt))
