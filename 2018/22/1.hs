-- stack --resolver lts-12.0 script
import qualified Data.Array as Array
import qualified Data.Char as Char
import qualified Data.IORef as IORef
import qualified Data.Map as Map

main = do
  [d, tx, ty]
    <- map read
    . words
    . map (\ c -> if Char.isDigit c then c else ' ')
    <$> readFile "input.txt"
  gic <- IORef.newIORef Map.empty
  elc <- IORef.newIORef Map.empty
  let t = (tx, ty :: Integer)
  regionTypes <- concat <$> mapM
    (\ y -> mapM
      (\ x -> let p = (x, y) in (,) p <$> regionType gic elc d t p)
      [0 .. tx])
    [0 .. ty]
  let cave = Array.array ((0, 0), t) (regionTypes)
  putStrLn (render cave)
  print (sum (map riskLevel (Array.elems cave)))

render cave = let
  ((xl, yl), (xh, yh)) = Array.bounds cave
  ys = [yl .. yh]
  xs = [xl .. xh]
  in unlines (map (\ y -> map (\ x -> cave Array.! (x, y)) xs) ys)

geologicIndex gic elc d t p = do
  m <- Map.lookup p <$> IORef.readIORef gic
  case m of
    Just x -> pure x
    Nothing -> do
      x <- case p of
        (0, 0) -> pure 0
        _ | p == t -> pure 0
        (x, 0) -> pure (x * 16807)
        (0, y) -> pure (y * 48271)
        (x, y) -> (*)
          <$> erosionLevel gic elc d t (x - 1, y)
          <*> erosionLevel gic elc d t (x, y - 1)
      IORef.modifyIORef gic $ Map.insert p x
      pure x

erosionLevel gic elc d t p = do
  m <- Map.lookup p <$> IORef.readIORef elc
  case m of
    Just x -> pure x
    Nothing -> do
      gi <- geologicIndex gic elc d t p
      let x = rem (gi + d) 20183
      IORef.modifyIORef elc $ Map.insert p x
      pure x

regionType gic elc d t p = do
  el <- erosionLevel gic elc d t p
  pure $ case rem el 3 of
    0 -> '.' -- rocky
    1 -> '=' -- wet
    2 -> '|' -- narrow

riskLevel x =
  case x of
    '.' -> 0
    '=' -> 1
    '|' -> 2
