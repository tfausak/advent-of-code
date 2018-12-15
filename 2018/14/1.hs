-- stack --resolver lts-12.0 script
import qualified Data.Sequence as Seq

main
  = putStrLn
  . foldr (\ d s -> toEnum (d + 48) : s) ""
  . Seq.take 10
  . Seq.drop 77201
  . fst
  . apply step 100000
  $ (Seq.fromList [3, 7], (0, 1))

apply f n x = if n == 0 then x else apply f (n - 1) (f x)

step (scores, (elf1, elf2)) = let
  Just score1 = Seq.lookup elf1 scores
  Just score2 = Seq.lookup elf2 scores
  newScores = scores Seq.>< Seq.fromList (digits (score1 + score2))
  in (newScores,
    ( mod (elf1 + 1 + score1) (Seq.length newScores)
    , mod (elf2 + 1 + score2) (Seq.length newScores)
    ))

digits n = if n == 0 then [n] else let
  go l x = if x == 0 then l else let
    (q, r) = quotRem x 10 in go (r : l) q
  in go [] n
