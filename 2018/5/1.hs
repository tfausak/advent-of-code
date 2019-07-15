-- stack --resolver ghc-8.6.5 script
module Main ( main ) where
import qualified Data.Char
import qualified Data.Maybe
import qualified Numeric.Natural

{-

This is a pithy Haskell solution:

> main = interact
>   $ show
>   . length
>   . foldr
>     (\ c s -> case s of
>       "" -> [c]
>       h : t -> if toLower c == toLower h && c /= h
>         then t else c : s)
>     ""
>   . filter isAlpha

But I thought it would be fun to write a more verbose, Elm-like solution
instead. Haskell doesn't have to be all polymorphic and point free!

-}


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


newtype Unit
  = Unit Char


data Polymer
  = Empty
  | Bond Unit Polymer


toUnit :: Char -> Maybe Unit
toUnit char =
  if Data.Char.isAlpha char
    then Just ( Unit char )
    else Nothing


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
isSameType (Unit x) (Unit y) =
  Data.Char.toLower x == Data.Char.toLower y


isOppositePolarity :: Unit -> Unit -> Bool
isOppositePolarity (Unit x) (Unit y) =
  x /= y


polymerLength :: Polymer -> Numeric.Natural.Natural
polymerLength polymer =
  case polymer of
    Empty ->
      0

    Bond _ rest ->
      1 + polymerLength rest
