{-#LANGUAGE ExistentialQuantification #-}
module Node where

import Data.IORef
import qualified Var as V
import Types

---------------------------------- Kind ---------------------------------------
-- Kind stores the computational information of each node.
-- https://github.com/janestreet/incremental/blob/master/src/kind.mli

-- Here I didn't use a IORef for Node because none of the methods in this module
-- will cause mutation
data Kind a =
    Const a
  | Invalid
  | forall b. Map (b -> a) (Node b) -- TODO: not sure about this
  | Uninitialized
  | Var (V.Var a)

instance Show (Kind a) where
  show (Const _)     = "Const"
  show Invalid       = "Invalid"
  show (Map _ _)     = "Map"
  show Uninitialized = "Uninitialized"
  show (Var _)       = "Var"

maxNumChildren :: Kind a -> Int
maxNumChildren (Const _)     = 0
maxNumChildren Invalid       = 0
maxNumChildren (Map _ _)     = 1
maxNumChildren Uninitialized = 0
maxNumChildren (Var _)       = 0

-- TODO: add error message
slowGetChild :: Kind a -> Index -> Packed_node
slowGetChild kind index =
  (iteriChildren kind (\_ n -> n)) !! index

-- Iterate through all the childrens with a function
-- no side effect
iteriChildren :: Kind a -> (Index -> Packed_node -> b) -> [b]
iteriChildren kind g =
  case kind of (Map _ n0) -> [g 0 (pack n0)]
               _          -> []

---------------------------------- NodeId -------------------------------------

data NodeId = NodeId { i :: IORef Int }

newId :: IO NodeId
newId = do
     fs <- newIORef 0
     return (NodeId {i = fs})

nextId :: NodeId -> IO Int
nextId node_id = do
     id <- readIORef (i node_id)
     writeIORef (i node_id) (id + 1)
     return id

---------------------------------- Node ---------------------------------------
data Node a = Node {
    nid      :: Int
  , kind     :: Kind a
    -- value :: a,
  , children :: [IORef (Node a)]
  , parents  :: [IORef (Node a)]
 }

newNode :: NodeId -> IO (Node a)
newNode node_id = do
       i <- nextId node_id
       return $ Node{ nid = i, kind = Invalid, children = [], parents=[] }

-- Checks whether n is a valid node
-- it is if and only if its kind is valid
isValid :: Node a -> Bool
isValid n = case kind n of
              Invalid -> False
              _       -> True

-- Checks whether m is a child of n
isChild :: IORef (Node a) -> Node a -> Bool
isChild child_ref node = child_ref `elem` (children node)

-- Checks whether m is a parent of n
isParent :: IORef (Node a) -> Node a -> Bool
isParent par_ref node = par_ref `elem` (parents node)

---------------------------------- Packed_node -------------------------------
data Packed_node

pack :: Node a -> Packed_node
pack = undefined


