-- stack --resolver lts-12.0 script
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Bits as Bits
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main = do
  (os, is) <- fmap
    ( Bifunctor.bimap
      ( getOperations Map.empty
      . Map.fromListWith Set.intersection
      . map (withPossibleOperations . parseSample . lines . Text.unpack)
      . Text.splitOn (Text.pack "\n\n")
      )
      (map parseInstruction . lines . Text.unpack . Text.stripStart)
    . Text.breakOn (Text.pack "\n\n\n\n")
    ) (Text.readFile "input.txt")
  print (get 0 (foldl
    (\r (n, a, b, c) -> execute (os Map.! n) a b c r)
    (0, 0, 0, 0 :: Int)
    is))

parseSample [r, i, s] =
  (parseRegisters r, parseInstruction i, parseRegisters s)

parseRegisters = four . read . drop 8

parseInstruction = four . map read . words

four [a, b, c, d] = (a, b, c, d)

getOperations os ns = case Maybe.mapMaybe getOperation (Map.toList ns) of
  [] -> os
  (n, o) : _ -> getOperations
    (Map.insert n o os)
    (Map.map (Set.delete o) (Map.delete n ns))

getOperation (n, os) = case Set.toList os of
  [o] -> Just (n, o)
  _ -> Nothing

withPossibleOperations (r, (n, a, b, c), s) =
  ( n
  , Set.filter
    (\o -> execute o a b c r == s)
    (Set.fromList [minBound .. maxBound])
  )

data Operation
  = ADDR | ADDI
  | MULR | MULI
  | BANR | BANI
  | BORR | BORI
  | SETR | SETI
  | GTIR | GTRI | GTRR
  | EQIR | EQRI | EQRR
  deriving (Bounded, Enum, Eq, Ord)

execute o a b c r = set c r $ case o of
  ADDR -> get a r + get b r
  ADDI -> get a r + b
  MULR -> get a r * get b r
  MULI -> get a r * b
  BANR -> get a r Bits..&. get b r
  BANI -> get a r Bits..&. b
  BORR -> get a r Bits..|. get b r
  BORI -> get a r Bits..|. b
  SETR -> get a r
  SETI -> a
  GTIR -> if a > get b r then 1 else 0
  GTRI -> if get a r > b then 1 else 0
  GTRR -> if get a r > get b r then 1 else 0
  EQIR -> if a == get b r then 1 else 0
  EQRI -> if get a r == b then 1 else 0
  EQRR -> if get a r == get b r then 1 else 0

set i (r0, r1, r2, r3) x = case i of
  0 -> (x, r1, r2, r3)
  1 -> (r0, x, r2, r3)
  2 -> (r0, r1, x, r3)
  3 -> (r0, r1, r2, x)

get i (r0, r1, r2, r3) = case i of
  0 -> r0
  1 -> r1
  2 -> r2
  3 -> r3
