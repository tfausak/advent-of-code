-- stack --resolver lts-12.0 script
import qualified Data.Map as Map

main
  = print
  . maximum
  . Map.elems
  . fst
  . solve
  . map read
  . words
  . filter (\ c -> c == ' ' || '0' <= c && c <= '9')
  =<< getContents

solve [ playerCount, lastMarble ] = let limit = lastMarble * 100 in foldr
  play
  ( Map.fromList $ zip [ 0 .. playerCount - 1 ] (repeat 0)
  , Deque [] [0]
  )
  [ limit, limit - 1 .. 1 ]

play marble ( scores, circle ) =
  if mod marble 23 == 0
  then let Just ( m, c ) = unsnoc $ rotate 7 circle in
    ( Map.insertWith (+) (mod marble $ Map.size scores) (marble + m) scores
    , rotate (-1) c
    )
  else ( scores, snoc marble $ rotate (-1) circle )

data Deque a = Deque [ a ] [ a ]

rotate n deque
  | n > 0 = rotate (n - 1) $ shiftRight deque
  | n < 0 = rotate (n + 1) $ shiftLeft deque
  | otherwise = deque

shiftLeft deque = maybe deque (uncurry snoc) $ uncons deque

shiftRight deque = maybe deque (uncurry cons) $ unsnoc deque

cons x (Deque l r) = Deque l (x : r)

uncons (Deque l r) = case r of
  h : t -> Just ( h, Deque l t )
  [] -> case reverse l of
    h : t -> Just ( h, Deque [] t )
    [] -> Nothing

snoc x (Deque l r) = Deque (x : l) r

unsnoc (Deque l r) = case l of
  h : t -> Just ( h, Deque t r )
  [] -> case reverse r of
    h : t -> Just ( h, Deque t [] )
    [] -> Nothing
