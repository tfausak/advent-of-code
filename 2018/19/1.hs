-- stack --resolver lts-12.0 script
import qualified Data.Bits as Bits
import qualified Data.Vector as Vector
import qualified Debug.Trace as Debug
import qualified Text.Printf as Printf

main = do
  declaration : instructions <- lines <$> readFile "input.txt"
  let initialState = State (read $ drop 4 declaration) $ Registers 0 0 0 0 0 0
  print initialState
  let finalState
        = run initialState
        . Vector.fromList
        $ map ((\ (op : xs) -> (op, map read xs)) . words) instructions
  print finalState

run :: State -> Vector.Vector (String, [Int]) -> State
run state instructions =
  let before = rs state
  in case instructions Vector.!? get (slot state) before of
    Nothing -> state
    Just (operation, [a, b, c]) ->
      let after = perform operation a b c before
      in run state { rs = inc (slot state) after } instructions

inc i r = set i (get i r + 1) r

data State = State { slot :: Int, rs :: Registers } deriving Show

data Registers = Registers { r0, r1, r2, r3, r4, r5 :: Int } deriving Show

get number = case number of
  0 -> r0
  1 -> r1
  2 -> r2
  3 -> r3
  4 -> r4
  5 -> r5

set number value registers = case number of
  0 -> registers { r0 = value }
  1 -> registers { r1 = value }
  2 -> registers { r2 = value }
  3 -> registers { r3 = value }
  4 -> registers { r4 = value }
  5 -> registers { r5 = value }

perform operation = case operation of
  "addr" -> addr
  "addi" -> addi
  "mulr" -> mulr
  "muli" -> muli
  "banr" -> banr
  "bani" -> bani
  "borr" -> borr
  "bori" -> bori
  "setr" -> setr
  "seti" -> seti
  "gtir" -> gtir
  "gtri" -> gtri
  "gtrr" -> gtrr
  "eqir" -> eqir
  "eqri" -> eqri
  "eqrr" -> eqrr

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
