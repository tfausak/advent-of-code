-- stack --resolver lts-12.0 script
import qualified Data.Set as Set

main
  = print
  . f Set.empty
  . scanl (+) 0
  . cycle
  . map read
  . lines
  . filter (/= '+')
  =<< getContents

f s l = case l of
  [] -> Nothing
  h : t -> if Set.member h s
    then Just h
    else f (Set.insert h s) t
