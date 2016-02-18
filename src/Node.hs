module Node where

import Data.IORef
import qualified Var as V
import qualified Kind as K
import Types

---------------------------------- NodeId -------------------------------------
data NodeId = NodeId { i :: IORef Int }

newId :: IO NodeId
newId = do
     fs <- newIORef 0
     return $ NodeId {i = fs}

nextId :: NodeId -> IO Int
nextId node_id = do
     id <- readIORef (i node_id)
     writeIORef (i node_id) (id + 1)
     return id

---------------------------------- Node ---------------------------------------
newNode :: NodeId -> IO (Node a)
newNode node_id = do
       i <- nextId node_id
       k <- newIORef (Invalid)
       p <- newIORef (EmptyNode)
       return $ Node { nid = i
                     , kind = k
                     , numParents = 0
                     , parent0 = p
                     , parent1AndBeyond = []
                     }

-- Checks whether n is a valid node
-- it is if and only if its kind is valid
isValid :: Node a -> IO Bool
isValid n = readIORef (kind n) >>=
  \k -> case k of Invalid -> return False
                  _       -> return True

isValid0 :: IORef (Node a) -> IO Bool
isValid0 ref = readIORef ref >>= \n -> isValid n

-- | Check whether some IORef Node is the parent of current node
isParent :: IORef (Node a) -> Node a -> Bool
isParent par_ref node
  | numParents node == 0 = False
  | otherwise = par_ref == (parent0 node)
             || par_ref `elem` (parent1AndBeyond node)

-- | 'iteriChildren' iterates all the child nodes
iteriChildren :: Node a -> (Index -> IORef (Node a) -> b) -> IO [b]
iteriChildren node g
  | numParents node == 0 = return []
  | otherwise = readIORef (kind node) >>= \k -> return $ K.iteriChildren k g

maxNumChildren :: Node a -> IO Int
maxNumChildren n = readIORef (kind n) >>= (return . K.maxNumChildren)

-- | 'getParent' returns the ref to the parent node
-- This is because later we might need to invalidate a node using this function
getParent :: Node a -> Index -> IORef (Node a)
getParent node i =
  if i == 0 then (parent0 node)
            else (parent1AndBeyond node) !! (i - 1)

-- | 'iteriParents' iterates all the parent nodes
iteriParents :: Node a -> (Index -> IORef (Node a) -> b) -> IO [b]
iteriParents node g
  | numParents node == 0 = return []
  | otherwise            = do
      let g0 =  g 0 (parent0 node)
          apply_g (i, ref) = g i ref
          rest = map apply_g (zip [1..] (parent1AndBeyond node))
      return (g0 : rest)

hasChild :: Node a -> IORef (Node a) -> IO Bool
hasChild node child = or <$> iteriChildren node (\_ ref -> ref == child)

hasInvalidChild :: Node a -> IO Bool
hasInvalidChild node = do
  res <- iteriChildren node (\_ ref -> not <$> (isValid0 ref)) >>= sequence
  return $ or res

hasParent :: Node a -> IORef (Node a) -> IO Bool
hasParent node parent = or <$> iteriParents node (\_ ref -> ref == parent)

-- shouldBeInvalidated ::
