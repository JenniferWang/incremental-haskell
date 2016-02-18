module Node where

import Data.IORef
import qualified Var as V
import qualified Kind as K
import Types
import Prelude hiding (id)

---------------------------------- NodeId -------------------------------------
data NodeId = NodeId { id :: IORef Int }

newId :: IO NodeId
newId = newIORef 0 >>= (return . NodeId)

nextId :: NodeId -> IO Int
nextId node_id = do
     i <- readIORef (id node_id)
     writeIORef (id node_id) (i + 1)
     return i

---------------------------------- Node ---------------------------------------
newNode :: NodeId -> IO (Node a)
newNode node_id = do
       i <- nextId node_id
       p <- newIORef (EmptyNode)
       return $ Node { nid = i
                     , kind = Invalid
                     , numParents = 0
                     , parent0 = p
                     , parent1AndBeyond = []
                     }

-- Checks whether n is a valid node
-- it is if and only if its kind is valid
isValid :: Node a -> Bool
isValid n = case (kind n) of Invalid -> False
                             _       -> True

isValid0 :: IORef (Node a) -> IO Bool
isValid0 ref = readIORef ref >>= \n -> return $ isValid n

-- | Check whether some IORef Node is the parent of current node
isParent :: IORef (Node a) -> Node a -> Bool
isParent par_ref node
  | numParents node == 0 = False
  | otherwise = par_ref == (parent0 node)
             || par_ref `elem` (parent1AndBeyond node)

-- | 'iteriChildren' iterates all the child nodes
iteriChildren :: Node a -> (Index -> IORef (Node a) -> b) -> [b]
iteriChildren node g
  | numParents node == 0 = []
  | otherwise = K.iteriChildren (kind node) g

maxNumChildren :: Node a -> Int
maxNumChildren = K.maxNumChildren . kind

-- | 'getParent' returns the ref to the parent node
-- This is because later we might need to invalidate a node using this function
getParent :: Node a -> Index -> IORef (Node a)
getParent node i =
  if i == 0 then (parent0 node)
            else (parent1AndBeyond node) !! (i - 1)

-- | 'iteriParents' iterates all the parent nodes
iteriParents :: Node a -> (Index -> IORef (Node a) -> b) -> [b]
iteriParents node g
  | numParents node == 0 = []
  | otherwise            = g0 : rest
    where g0               =  g 0 (parent0 node)
          apply_g (i, ref) = g i ref
          rest             = map apply_g (zip [1..] (parent1AndBeyond node))

hasChild :: Node a -> IORef (Node a) -> Bool
hasChild node child = or $ iteriChildren node (\_ ref -> ref == child)

hasInvalidChild :: Node a -> IO Bool
hasInvalidChild node = or <$>
  (sequence $ iteriChildren node (\_ ref -> not <$> (isValid0 ref)))

hasInvalidChild0 :: IORef (Node a) -> IO Bool
hasInvalidChild0 ref = readIORef ref >>= hasInvalidChild

hasParent :: Node a -> IORef (Node a) -> Bool
hasParent node parent = or $ iteriParents node (\_ ref -> ref == parent)

shouldBeInvalidated :: Node a -> IO Bool
shouldBeInvalidated n =
  case (kind n) of Map _ n_ref -> hasInvalidChild0 n_ref
                   _           -> return False

setKind :: IORef (Node a) -> Kind a -> IO ()
setKind n_ref k = modifyIORef' n_ref (\n -> n{ kind = k })


-- | 'addParent' adds the parent node to the child node's parent list
-- Here the parent is added to the beginning of the parents list
-- The OCaml version takes the child_index for performance reason.
-- https://github.com/janestreet/incremental/blob/master/src/node.ml#L519
addParent :: IORef (Node a) -> IORef (Node a) -> IO ()
addParent child parent = do
  c <- readIORef child
  if (numParents c) == 0
     then writeIORef child c{ parent0 = parent, numParents = 1}
     else let new_pars = parent: (parent1AndBeyond c)
              new_num  = numParents c + 1
           in writeIORef child c{ parent1AndBeyond = new_pars, numParents = new_num }

removeParent :: IORef (Node a) -> IORef (Node a) -> IO ()
removeParent = undefined


test :: IO ()
test = do
  id <- newId
  n1 <- newNode id
  n2 <- newNode id
  n3 <- newNode id
  ref1 <- newIORef n1
  ref2 <- newIORef n2
  ref3 <- newIORef n3
  addParent ref2 ref3
  addParent ref2 ref1
  setKind ref1 Uninitialized
  n1' <- readIORef ref1
  n2' <- readIORef ref2
  n2_par <- readIORef (head $ parent1AndBeyond n2')
  putStrLn $ "now n1 kind is " ++ show (kind n1')
  putStrLn $ "now n2_par kind is " ++ show (kind n2_par)

---------------------------------- Helper --------------------------------------

