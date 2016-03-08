module ArrayFold where

import Control.Monad (foldM)

import Types
import Utils
import Node (valueExn)

compute :: (Eq a) => (b -> a -> b) -> b -> [NodeRef a] -> StateIO b
compute f init ns =  foldM go init ns
  where go acc node = do
          n <- readNodeRef node
          return $ f acc (valueExn n)
