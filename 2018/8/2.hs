-- stack --resolver lts-12.0 script
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State as State

main
  = print
  . value
  . State.evalState getNode
  . map read
  . words
  =<< getContents

value node = if null $ children node
  then sum $ metadata node
  else sum . map (maybe 0 value . at (children node)) $ metadata node

at list index = case list of
  first : rest
    | index == 1 -> Just first
    | index > 1 -> at rest (index - 1)
  _ -> Nothing

data Node = Node { children :: [ Node ], metadata :: [ Int ] }

getNode = do
  n <- getInt
  m <- getInt
  Node <$> Monad.replicateM n getNode <*> Monad.replicateM m getInt

getInt = do
  first : rest <- State.get
  State.put rest
  pure first
