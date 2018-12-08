-- stack --resolver lts-12.0 script
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State as State

main
  = print
  . sum
  . allMetadata
  . State.evalState getNode
  . map read
  . words
  =<< getContents

allMetadata node = metadata node ++ concatMap allMetadata (children node)

data Node = Node { children :: [ Node ], metadata :: [ Int ] } deriving Show

getNode = do
  n <- getInt
  m <- getInt
  Node <$> Monad.replicateM n getNode <*> Monad.replicateM m getInt

getInt = do
  first : rest <- State.get
  State.put rest
  pure first
