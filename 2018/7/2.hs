-- stack --resolver lts-12.0 script
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.ParserCombinators.ReadP as Parse

main
  = print
  . solve 5
  . map (\ ( step, steps ) -> State step False (60 + fromEnum step - 64) steps)
  . Map.toList
  . Map.fromListWith Set.union
  . concatMap (\ requirement ->
    [ ( after requirement, Set.singleton $ before requirement )
    , ( before requirement, Set.empty )
    ])
  . map read
  . lines
  =<< getContents

solve workers list = case List.partition ((== 0) . time) list of
  ( [], [] ) -> 0
  ( [], _ ) -> let
    ( active, inactive ) = List.partition working list
    activeSteps
      = Set.fromList
      . map step
      . take workers
      $ active ++ filter (Set.null . steps) inactive
    update state = let isActive = Set.member (step state) activeSteps in state
      { working = isActive
      , time = if isActive then time state - 1 else time state
      }
    in 1 + solve workers (map update list)
  ( remove, keep ) -> solve workers $ map
    (\ state -> state
      { steps = Set.difference (steps state) . Set.fromList $ map step remove
      })
    keep

data State = State
  { step :: Char
  , working :: Bool
  , time :: Int
  , steps :: Set.Set Char
  }

data Requirement = Requirement { before, after :: Char }

instance Read Requirement where
  readsPrec _ = Parse.readP_to_S $ Requirement
    <$> (Parse.string "Step " *> Parse.get <* Parse.string " must be finished before ")
    <*> (Parse.string "step " *> Parse.get <* Parse.string " can begin.")
