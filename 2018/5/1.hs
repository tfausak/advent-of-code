-- stack --resolver ghc-8.6.5 script
module Main ( main ) where
import qualified Data.Char
import qualified Data.Maybe
import qualified Numeric.Natural

-- This is a pithy Haskell solution:
--
-- > import Data.Char
-- > main = interact
-- >   $ show
-- >   . length
-- >   . foldr
-- >     (\ c s -> case s of
-- >       "" -> [c]
-- >       h : t -> if toLower c == toLower h && c /= h
-- >         then t else c : s)
-- >     ""
-- >   . filter isAlpha
--
-- But I thought it would be fun to write a more verbose, Elm-like solution
-- instead. Haskell doesn't have to be all polymorphic and point free!


main :: IO ()
main =
  interact (\ contents ->
    contents
      |> Data.Maybe.mapMaybe toUnit
      |> foldr react Empty
      |> polymerLength
      |> show)


(|>) :: i -> (i -> o) -> o
x |> f =
  f x


data Unit
  = Unit Type Polarity


newtype Type
  = Type Char


data Polarity
  = Positive
  | Negative


data Polymer
  = Empty
  | Bond Unit Polymer


toUnit :: Char -> Maybe Unit
toUnit char =
  case toType char of
    Just type_ ->
      let polarity = toPolarity char
      in Just ( Unit type_ polarity )

    Nothing ->
      Nothing


toType :: Char -> Maybe Type
toType char =
  if Data.Char.isAlpha char then
    Just ( Type ( Data.Char.toUpper char ) )
  else
    Nothing


toPolarity :: Char -> Polarity
toPolarity char =
  if Data.Char.isUpper char then
    Positive
  else
    Negative


react :: Unit -> Polymer -> Polymer
react unit polymer =
  case polymer of
    Empty ->
      Bond unit polymer

    Bond first rest ->
      if doesReact unit first then
        rest
      else
        Bond unit polymer


doesReact :: Unit -> Unit -> Bool
doesReact x y =
  isSameType x y && isOppositePolarity x y


isSameType :: Unit -> Unit -> Bool
isSameType (Unit (Type x) _) (Unit (Type y) _) =
  x == y


isOppositePolarity :: Unit -> Unit -> Bool
isOppositePolarity (Unit _ x) (Unit _ y) =
  case ( x, y ) of
    ( Positive, Negative ) ->
      True

    ( Negative, Positive ) ->
      True

    _ ->
      False


polymerLength :: Polymer -> Numeric.Natural.Natural
polymerLength polymer =
  case polymer of
    Empty ->
      0

    Bond _ rest ->
      1 + polymerLength rest
