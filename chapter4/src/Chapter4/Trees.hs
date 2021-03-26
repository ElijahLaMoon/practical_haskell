module Chapter4.Trees where

import qualified Data.Tree as T

data Tree   a = Node { rootLabel :: a, subforest :: Forest a }
type Forest a = [Tree a]

tree = Node 1 [ Node 2 [Node 3 [], Node 4 [], Node 5 []]
              , Node 6 []
              ]