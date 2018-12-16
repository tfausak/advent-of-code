-- stack --resolver lts-12.0 script
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Bits as Bits
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Text.ParserCombinators.ReadP as Parse

main = do
  samples <- fmap
    ( filter (>= 3)
    . map (length . possibilities . parseSample . lines . Text.unpack)
    . Text.splitOn (Text.pack "\n\n")
    . fst
    . Text.breakOn (Text.pack "\n\n\n\n")
    ) (Text.readFile "input.txt")
  print $ length $ samples

possibilities :: Sample -> [Operation]
possibilities (Sample (Before before) (Instruction _ a b c) (After after)) =
  filter
    (\operation -> perform operation a b c before == after)
    [minBound .. maxBound]

data Operation
  = ADDR
  | ADDI
  | MULR
  | MULI
  | BANR
  | BANI
  | BORR
  | BORI
  | SETR
  | SETI
  | GTIR
  | GTRI
  | GTRR
  | EQIR
  | EQRI
  | EQRR
  deriving (Bounded, Enum, Show)

perform :: Operation -> Int -> Int -> Int -> Registers -> Registers
perform op = case op of
  ADDR -> addr
  ADDI -> addi
  MULR -> mulr
  MULI -> muli
  BANR -> banr
  BANI -> bani
  BORR -> borr
  BORI -> bori
  SETR -> setr
  SETI -> seti
  GTIR -> gtir
  GTRI -> gtri
  GTRR -> gtrr
  EQIR -> eqir
  EQRI -> eqri
  EQRR -> eqrr

addr a b c r = set c (get a r + get b r) r
addi a b c r = set c (get a r + b) r
mulr a b c r = set c (get a r * get b r) r
muli a b c r = set c (get a r * b) r
banr a b c r = set c (get a r Bits..&. get b r) r
bani a b c r = set c (get a r Bits..&. b) r
borr a b c r = set c (get a r Bits..|. get b r) r
bori a b c r = set c (get a r Bits..|. b) r
setr a _ c r = set c (get a r) r
seti a _ c r = set c a r
gtir a b c r = set c (if a > get b r then 1 else 0) r
gtri a b c r = set c (if get a r > b then 1 else 0) r
gtrr a b c r = set c (if get a r > get b r then 1 else 0) r
eqir a b c r = set c (if a == get b r then 1 else 0) r
eqri a b c r = set c (if get a r == b then 1 else 0) r
eqrr a b c r = set c (if get a r == get b r then 1 else 0) r

get :: Int -> Registers -> Int
get i = case i of
  0 -> r0
  1 -> r1
  2 -> r2
  3 -> r3

set :: Int -> Int -> Registers -> Registers
set i x r = case i of
  0 -> r { r0 = x }
  1 -> r { r1 = x }
  2 -> r { r2 = x }
  3 -> r { r3 = x }

parseSample [before, instruction, after] =
  Sample (read before) (read instruction) (read after)

data Sample = Sample Before Instruction After deriving (Eq, Show)

newtype Before = Before Registers deriving (Eq, Show)

instance Read Before where
  readsPrec n = Parse.readP_to_S (Before
    <$> (Parse.string "Before: " *> Parse.readS_to_P (readsPrec n)))

newtype After = After Registers deriving (Eq, Show)

instance Read After where
  readsPrec n = Parse.readP_to_S (After
    <$> (Parse.string "After:  " *> Parse.readS_to_P (readsPrec n)))

data Registers = Registers { r0, r1, r2, r3 :: Int } deriving (Eq, Show)

instance Read Registers where
  readsPrec n = Parse.readP_to_S (Registers
    <$> (Parse.char '[' *> Parse.readS_to_P (readsPrec n))
    <*> (Parse.string ", " *> Parse.readS_to_P (readsPrec n))
    <*> (Parse.string ", " *> Parse.readS_to_P (readsPrec n))
    <*> (Parse.string ", " *> Parse.readS_to_P (readsPrec n) <* Parse.char ']'))

data Instruction = Instruction Int Int Int Int deriving (Eq, Show)

instance Read Instruction where
  readsPrec n = Parse.readP_to_S (Instruction
    <$> Parse.readS_to_P (readsPrec n)
    <*> (Parse.char ' ' *> Parse.readS_to_P (readsPrec n))
    <*> (Parse.char ' ' *> Parse.readS_to_P (readsPrec n))
    <*> (Parse.char ' ' *> Parse.readS_to_P (readsPrec n)))
