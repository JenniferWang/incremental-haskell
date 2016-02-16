module Node (
    Node(..)
  , Packed_node
  , pack
  ) where

data Node a = Node {
    getNode :: a
  }

data Packed_node

pack :: Node a -> Packed_node
pack = undefined

