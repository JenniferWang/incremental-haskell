module Scope where

import Lens.Simple

import Types
import Utils

addNode :: Eq a => Scope -> NodeRef a -> StateIO ()
addNode Top               _  = return ()
addNode (Bound bind_root) nf = do
  putStrLnT $ "Add node" ++ show nf ++ " to bind " ++ show bind_root
  modifyNodeRef bind_root (\n -> n & kind %~ (addNodeForBind nf))

addNodeForBind :: (Eq a, Eq b) => NodeRef b -> Kind a -> Kind a
addNodeForBind nf b@(Bind _ _ _ nodes_on_rhs) =
  b{ nodesCreatedInScope = (pack nf): nodes_on_rhs }
addNodeForBind _ _ = error "Scope.addNodeForBind wrong type of bind node in scope"

