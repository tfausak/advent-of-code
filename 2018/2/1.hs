-- stack --resolver lts-12.0 script
import Control.Arrow ((&&&))
import qualified Data.Map as Map

main
  = print
  . uncurry (*)
  . (count 2 &&& count 3)
  . map (Map.elems . foldr (\ x -> Map.insertWith (+) x 1) Map.empty)
  . lines
  =<< getContents

count n = length . filter (elem n)
