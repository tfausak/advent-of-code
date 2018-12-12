-- stack --resolver lts-12.0 script
import qualified Data.Array as Array
import Data.IORef
import Debug.Trace

main = do
  let
    x1 = 1
    x2 = 300
    y1 = x1
    y2 = x2
    n = 7139
    xs = Array.array ( ( x1, y1 ), ( x2, y2 ) )
      . flip concatMap [ y1 .. y2 ] $ \ y ->
        flip map [ x1 .. x2 ] $ \ x ->
          ( ( x, y ), level n x y )
  ref <- newIORef minBound
  flip mapM_ [ y1 .. y2 ] $ \ y -> do
    print y
    flip mapM_ [ x1 .. x2 ] $ \ x -> do
      print x
      flip mapM_ [ 1 .. min x2 y2 - max x y ] $ \ s ->
        modifyIORef' ref $ \ ( n', x', y', s' ) ->
          let n = power xs x y s
          in if n > n' then ( n, x, y, s ) else ( n', x', y', s' )
  print . (\ x -> x :: ( Int, Int, Int, Int )) =<< readIORef ref

power xs x y s =
  sum . flip concatMap [ y .. y + s - 1 ] $ \ y' ->
    flip map [ x .. x + s - 1 ] $ \ x' ->
      xs Array.! ( x', y' )

level n x y = mod (div ((((x + 10) * y) + n) * (x + 10)) 100) 10 - 5
