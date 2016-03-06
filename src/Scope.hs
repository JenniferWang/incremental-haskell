module Scope where

import Lens.Simple

import Types
import Utils

addNode :: Eq a => Scope -> NodeRef a -> StateIO ()
addNode Top               _  = return ()
addNode (Bound bind_root) nf = do
   modifyNodeRef bind_root (\n -> n & kind %~ (addNodeForBind nf))

addNodeForBind :: (Eq a, Eq b) => NodeRef b -> Kind a -> Kind a
addNodeForBind nf b@(Bind _ _ _ nodes_on_rhs) =
  let old_n = nodesCreatedInScope b
   in b{ nodesCreatedInScope = (pack nf): old_n }
addNodeForBind _ _ = error "Scope.addNodeForBind wrong type of bind node in scope"

