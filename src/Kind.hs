{-#LANGUAGE ExistentialQuantification #-}
-- Kind stores the computational information of each node.
-- https://github.com/janestreet/incremental/blob/master/src/kind.mli

-- Here I didn't use a IORef for Node because none of the methods in this module
-- will cause mutation

module Kind (
    Kind(..)
  , iteri_children
  , slow_get_child
  , max_num_children
  , test
  ) where

import qualified Node as N
import qualified Var as V
import Data.Maybe

-- TODO: move it to some type.hs?
type Index = Int

data Kind a =
    Const a
  | Invalid
  | forall b. Map (b -> a) (N.Node b) -- TODO: not sure about this
  | Uninitialized
  | Var (V.Var a)

instance Show (Kind a) where
  show (Const _)     = "Const"
  show Invalid       = "Invalid"
  show (Map _ _)     = "Map"
  show Uninitialized = "Uninitialized"
  show (Var _)       = "Var"

max_num_children :: Kind a -> Int
max_num_children (Const _)     = 0
max_num_children Invalid       = 0
max_num_children (Map _ _)     = 1
max_num_children Uninitialized = 0
max_num_children (Var _)       = 0

-- TODO: add error message
slow_get_child :: Kind a -> Index -> N.Packed_node
slow_get_child kind index = fromJust $
  (iteri_children kind (\_ n -> Just n)) !! index

-- Iterate through all the childrens with a function
-- no side effect
iteri_children :: Kind a -> (Index -> N.Packed_node -> Maybe b) -> [Maybe b]
iteri_children kind g =
  case kind of (Map _ n0) -> [g 0 (N.pack n0)]
               _          -> []

test :: (b -> a) -> N.Node b -> Kind a
test f n = Const $ f (N.getNode n)
