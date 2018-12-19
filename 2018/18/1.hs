-- stack --resolver lts-12.0 script
import qualified Control.Monad as Monad
import qualified Data.Array as Array
import qualified Data.IORef as IORef
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified System.Environment as Environment

main = do
  limit <- pure . read . Maybe.fromMaybe "10" . Maybe.listToMaybe =<< Environment.getArgs
  rows <- lines <$> readFile "input.txt"
  let
    height = length rows
    width = length (head rows)
    initial = Array.array ((1, 1), (width, height))
      (flip concatMap (zip [1 ..] rows)
        (\ (y, row) -> flip map (zip [1 ..] row)
          (\ (x, cell) -> ((x, y), cell))))

  areaRef <- IORef.newIORef initial
  freqRef <- IORef.newIORef Map.empty

  Monad.forM_ [1 .. limit] $ \ n -> do
    IORef.modifyIORef areaRef tick
    area <- IORef.readIORef areaRef
    let result = uncurry (*) $ reduce area
    IORef.modifyIORef freqRef $ Map.insertWith (+) result 1
    freq <- Map.findWithDefault 0 result <$> IORef.readIORef freqRef
    putStrLn $ show n <> "\t" <> show result <> "\t" <> show freq

render area = let ((xl, yl), (xh, yh)) = Array.bounds area in
  unlines $ map (\ y -> map (\ x -> area Array.! (x, y)) [xl .. xh]) [yl .. yh]

reduce area = foldr (\ i (ts, ls) -> case area Array.! i of
  '.' -> (ts, ls)
  '|' -> (ts + 1, ls)
  '#' -> (ts, ls + 1)) (0, 0) $ Array.indices area

tick area
  = Array.accum (flip const) area
  . map (\ i -> (i, change area i (area Array.! i)))
  $ Array.indices area

change area (x, y) acre =
  let acres = map (area Array.!) (adjacentTo area (x, y))
  in case acre of
    '.' -> if count (== '|') acres >= 3 then '|' else '.'
    '|' -> if count (== '#') acres >= 3 then '#' else '|'
    '#' -> if any (== '#') acres && any (== '|') acres then '#' else '.'

adjacentTo area (x, y) = filter (inBounds area)
  [ (x - 1, y - 1), (x + 0, y - 1), (x + 1, y - 1)
  , (x - 1, y + 0),                 (x + 1, y + 0)
  , (x - 1, y + 1), (x + 0, y + 1), (x + 1, y + 1)
  ]

inBounds area (x, y) =
  let ((xl, yl), (xh, yh)) = Array.bounds area
  in xl <= x && x <= xh && yl <= y && y <= yh

count p = length . filter p
