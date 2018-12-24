-- stack --resolver lts-13.0 script
import qualified Control.Arrow as Arrow
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Text.ParserCombinators.ReadP as Parse
import qualified Text.Read as Read

main
  = print
  . sum
  . map groupUnits
  . concatMap Map.elems
  . Map.elems
  . stateArmies
  . doCombat
  . Maybe.fromJust
  . toState
  . map (map read . drop 1 . lines . Text.unpack)
  . Text.splitOn (Text.pack "\n\n")
  . Text.pack
  =<< readFile "input.txt"

doCombat state = if combatIsOver state then state else doCombat $ fight state

combatIsOver = any Map.null . stateArmies

fight state = doAttacks state $ findTargets state

doAttacks state
  = foldr doAttack state
  . List.sortOn (groupInitiative . snd)
  . Map.toList

doAttack (d, a) state =
  case lookupGroup a state of
    Nothing -> state
    Just attacker -> case lookupGroup d state of
      Nothing -> state
      Just defender -> let
        damage = attackDamage attacker defender
        killed = quot damage $ groupHitPoints defender
        newDefender = defender { groupUnits = groupUnits defender - killed }
        update = if groupUnits newDefender < 1 then
            Map.delete $ groupNumber newDefender
          else
            Map.insert (groupNumber newDefender) newDefender
        in state { stateArmies = Map.adjust update (groupArmy newDefender) $ stateArmies state }

lookupGroup group
  = Map.lookup (groupNumber group)
  . Map.findWithDefault Map.empty (groupArmy group)
  . stateArmies

findTargets state
  = foldr (insertTarget state) Map.empty
  . List.sortOn sortTargets
  $ stateGroups state

sortTargets = effectivePower Arrow.&&& groupInitiative

effectivePower group = groupUnits group * groupAttackDamage group

insertTarget state attacker targets =
  case findTarget attacker targets state of
    Nothing -> targets
    Just defender -> Map.insert defender attacker targets

findTarget attacker targets
  = Maybe.listToMaybe
  . List.sortOn (sortTarget attacker)
  . filter (keepTarget targets attacker)
  . Map.elems
  . Map.findWithDefault Map.empty (otherArmy $ groupArmy attacker)
  . stateArmies

keepTarget targets attacker defender
  = damageModifier attacker defender /= Just ModifierImmune
  && not (Map.member defender targets)

sortTarget attacker
  = Ord.Down
  . (attackDamage attacker Arrow.&&& effectivePower Arrow.&&& groupInitiative)

attackDamage attacker defender = case damageModifier attacker defender of
  Just ModifierImmune -> 0
  Nothing -> effectivePower attacker
  Just ModifierWeak -> 2 * effectivePower attacker

damageModifier attacker defender =
  if Set.member (groupAttackType attacker) $ groupImmunities defender then
    Just ModifierImmune
  else if Set.member (groupAttackType attacker) $ groupWeaknesses defender then
    Just ModifierWeak
  else
    Nothing

otherArmy army = case army of
  ArmyInfection -> ArmyImmuneSystem
  ArmyImmuneSystem -> ArmyInfection

stateGroups = concatMap Map.elems . Map.elems . stateArmies

newtype State = State
  { stateArmies :: Map.Map Army (Map.Map Int Group)
  } deriving (Eq, Ord, Show)

toState = fmap (State . foldr insertGroup Map.empty) . toGroups

insertGroup group groups = Map.insertWith
  Map.union (groupArmy group) (Map.singleton (groupNumber group) group) groups

toGroups armies = case armies of
  [immuneSystem, infection] -> Just $ mappend
    (map (toGroup ArmyImmuneSystem) $ zip [1 ..] immuneSystem)
    (map (toGroup ArmyInfection) $ zip [1 ..] infection)
  _ -> Nothing

data Group = Group
  { groupArmy :: Army
  , groupNumber :: Int
  , groupUnits :: Int
  , groupHitPoints :: Int
  , groupWeaknesses :: Set.Set AttackType
  , groupImmunities :: Set.Set AttackType
  , groupAttackDamage :: Int
  , groupAttackType :: AttackType
  , groupInitiative :: Int
  } deriving (Eq, Ord, Show)

toGroup army (number, input) = Group
  { groupArmy = army
  , groupNumber = number
  , groupUnits = inputUnits input
  , groupHitPoints = inputHitPoints input
  , groupWeaknesses = getSpecialsWith ModifierWeak input
  , groupImmunities = getSpecialsWith ModifierImmune input
  , groupAttackDamage = inputAttackDamage input
  , groupAttackType = inputAttackType input
  , groupInitiative = inputInitiative input
  }

getSpecialsWith modifier
  = Set.fromList
  . concatMap specialAttackTypes
  . filter ((== modifier) . specialModifier)
  . inputSpecials

data Army
  = ArmyInfection
  | ArmyImmuneSystem
  deriving (Eq, Ord, Show)

data Input = Input
  { inputUnits :: Int
  , inputHitPoints :: Int
  , inputSpecials :: [Special]
  , inputAttackDamage :: Int
  , inputAttackType :: AttackType
  , inputInitiative :: Int
  } deriving (Eq, Show)

instance Read Input where
  readsPrec _ = Parse.readP_to_S parseInput

parseInput = Input
  <$> parseInt
  <*> (Parse.string " units each with " *> parseInt)
  <*> (Parse.string " hit points" *> Parse.option [] parseSpecials)
  <*> (Parse.string " with an attack that does " *> parseInt)
  <*> (Parse.char ' ' *> parseAttackType)
  <*> (Parse.string " damage at initiative " *> parseInt)

parseSpecials
  = Parse.string " ("
  *> Parse.sepBy1 parseSpecial (Parse.string "; ")
  <* Parse.char ')'

data Special = Special
  { specialModifier :: Modifier
  , specialAttackTypes :: [AttackType]
  } deriving (Eq, Ord, Show)

parseSpecial = Special
  <$> parseModifier
  <*> (Parse.string " to " *> Parse.sepBy1 parseAttackType (Parse.string ", "))

data Modifier
  = ModifierImmune
  | ModifierWeak
  deriving (Eq, Ord, Show)

parseModifier = Parse.choice
  [ ModifierImmune <$ Parse.string "immune"
  , ModifierWeak <$ Parse.string "weak"
  ]

data AttackType
  = AttackTypeBludgeoning
  | AttackTypeCold
  | AttackTypeFire
  | AttackTypeRadiation
  | AttackTypeSlashing
  deriving (Eq, Ord, Show)

parseAttackType = Parse.choice
  [ AttackTypeBludgeoning <$ Parse.string "bludgeoning"
  , AttackTypeCold <$ Parse.string "cold"
  , AttackTypeFire <$ Parse.string "fire"
  , AttackTypeRadiation <$ Parse.string "radiation"
  , AttackTypeSlashing <$ Parse.string "slashing"
  ]

parseInt = either fail pure . Read.readEither =<< Parse.munch1 Char.isDigit
