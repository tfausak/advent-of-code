-- stack --resolver lts-12.0 script
import qualified Data.Maybe as Maybe
import qualified Data.List as List

main
  = print
  . fmap snd
  . Maybe.listToMaybe
  . filter ((== 1) . fst)
  . map (uncurry ham)
  . pairs
  . lines
  =<< getContents

pairs xs = do
  x : ys <- List.tails xs
  y <- ys
  pure ( x, y )

ham xs ys = foldr
  (\ (x, y) (n, zs) -> if x == y then (n, x : zs) else (n + 1, zs))
  (0, "")
  (zip xs ys)
