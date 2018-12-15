-- stack --resolver lts-12.0 script
import qualified Data.List as List
import qualified Data.Sequence as Seq

main
  = print
  . length
  . takeWhile (not . List.isPrefixOf [0, 7, 7, 2, 0, 1])
  . List.tails
  $ 3 : 7 : generate 0 1 (Seq.fromList [3, 7])

generate elf1 elf2 scores = let
  score1 = Seq.index scores elf1
  score2 = Seq.index scores elf2
  digits = toDigits (score1 + score2)
  newScores = scores <> Seq.fromList digits
  newElf1 = mod (elf1 + score1 + 1) (length newScores)
  newElf2 = mod (elf2 + score2 + 1) (length newScores)
  in digits <> generate newElf1 newElf2 newScores

toDigits n =
  let (q, r) = quotRem n 10 in
  if q == 0 then [r] else q : toDigits r
