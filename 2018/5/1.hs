-- stack --resolver lts-12.0 script
import qualified Data.Char as Char

main
  = print
  . length
  . foldr
    (\ c1 s1 -> case s1 of
      [] -> [ c1 ]
      c2 : s2 -> if c1 /= c2 && Char.toLower c1 == Char.toLower c2
        then s2
        else c1 : s1)
    []
  . filter Char.isAlpha
  =<< getContents
