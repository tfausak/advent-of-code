-- stack --resolver lts-12.0 script
import qualified Data.Map as Map

main
  = print
  . maximum
  . Map.elems
  . scores
  . (\ [ x, y ] -> solve (State [0] 1 0 Map.empty x y))
  . map read
  . words
  . filter (\ c -> ' ' <= c && c <= '9')
  =<< getContents

solve state =
  if marbleNumber state > lastMarble state
    then state
    else if mod (marbleNumber state) 23 == 0
      then let
        rawIndex = currentIndex state - 7
        index = if rawIndex < 0 then length (circle state) + rawIndex else rawIndex
        ( marble, newCircle ) = removeAt index (circle state)
        in solve state
        { circle = newCircle
        , scores = Map.insertWith
          (+)
          (mod (marbleNumber state) (playerCount state))
          (marbleNumber state + marble)
          (scores state)
        , marbleNumber = marbleNumber state + 1
        , currentIndex = index
        }
      else let
        index = findIndex (length (circle state)) (currentIndex state)
        in solve state
        { circle = insertAt index (marbleNumber state) (circle state)
        , marbleNumber = marbleNumber state + 1
        , currentIndex = index
        }

removeAt index list = case splitAt index list of
  ( before, element : after ) -> ( element, before ++ after )

findIndex n i = case ( n, i ) of
  ( 1, 0 ) -> 1
  _ -> let j = i + 2 in if j > n then mod j n else j

insertAt index element list = if index <= 0
  then element : list
  else case list of
    [] -> [ element ]
    first : rest -> first : insertAt (index - 1) element rest

data State = State
  { circle :: [ Int ]
  , marbleNumber :: Int
  , currentIndex :: Int
  , scores :: Map.Map Int Int
  , playerCount :: Int
  , lastMarble :: Int
  }
