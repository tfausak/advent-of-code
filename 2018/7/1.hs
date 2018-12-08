-- stack --resolver lts-12.0 script
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.ParserCombinators.ReadP as Parse

main
  = putStrLn
  . solve
  . Map.toList
  . Map.fromListWith Set.union
  . concatMap (\ requirement ->
    [ ( after requirement, Set.singleton $ before requirement )
    , ( before requirement, Set.empty )
    ])
  . map read
  . lines
  =<< getContents

solve list = case list of
  [ ( next, _ ) ] -> [ next ]
  _ -> let
    next = fst . minimum $ filter (Set.null . snd) list
    rest
      = map (\ ( step, steps ) -> ( step, Set.delete next steps ))
      $ filter ((/= next) . fst) list
    in next : solve rest

data Requirement = Requirement { before, after :: Char } deriving Show

instance Read Requirement where
  readsPrec _ = Parse.readP_to_S $ Requirement
    <$> (Parse.string "Step " *> Parse.get <* Parse.string " must be finished before ")
    <*> (Parse.string "step " *> Parse.get <* Parse.string " can begin.")
