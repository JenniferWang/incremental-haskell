module Node where

import Data.IORef
import Lens.Simple
import qualified Var as V
import qualified Kind as K
import Types
import Prelude hiding (id)
import Data.Maybe(fromJust)

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
       let p_info = ParentInfo 0 Nothing []
       return $ Node { nid = i
                     , _kind = Invalid
                     , _pinfo = p_info
                     }

-- Checks whether n is a valid node
-- it is if and only if its kind is valid
isValid :: Node a -> Bool
isValid n = case (n^.kind) of Invalid -> False
                              _       -> True

isValid0 :: PackedNode -> IO Bool
isValid0 (PackedNode ref) = fmap isValid $ readIORef ref

-- | Check whether some IORef Node is the parent of current node
isParent :: PackedNode -> Node a -> Bool
isParent par_ref node
  | node^.pinfo.numPar == 0 = False
  | otherwise = par_ref == fromJust (node^.pinfo.par0)
             || par_ref `elem` (node^.pinfo.par1AndBeyond)

-- | 'iteriChildren' iterates all the child nodes
iteriChildren :: Node a -> (Index -> PackedNode -> b) -> [b]
iteriChildren node g
  | node^.pinfo.numPar == 0 = []
  | otherwise = K.iteriChildren (node^.kind) g

maxNumChildren :: Node a -> Int
maxNumChildren = K.maxNumChildren . _kind

-- | 'getParent' returns the ref to the parent node
-- This is because later we might need to invalidate a node using this function
getParent :: Node a -> Index -> PackedNode
getParent node i =
  if i == 0 then (fromJust $ node^.pinfo.par0)
            else (node^.pinfo.par1AndBeyond) !! (i - 1)

-- | 'iteriParents' iterates all the parent nodes
iteriParents :: Node a -> (Index -> PackedNode -> b) -> [b]
iteriParents node g
  | node^.pinfo.numPar == 0 = []
  | otherwise            = g0 : rest
    where g0               = g 0 (fromJust $ node^.pinfo.par0)
          apply_g (i, ref) = g i ref
          rest             = map apply_g (zip [1..] (node^.pinfo.par1AndBeyond))

hasChild :: Node a -> PackedNode -> Bool
hasChild node child = or $ iteriChildren node (\_ ref -> ref == child)

hasInvalidChild :: Node a -> IO Bool
hasInvalidChild node = or <$>
  (sequence $ iteriChildren node (\_ ref -> not <$> (isValid0 ref)))

hasInvalidChild0 :: IORef (Node a) -> IO Bool
hasInvalidChild0 ref = readIORef ref >>= hasInvalidChild

hasParent :: Node a -> PackedNode -> Bool
hasParent node parent = or $ iteriParents node (\_ ref -> ref == parent)

shouldBeInvalidated :: Node a -> IO Bool
shouldBeInvalidated n =
  case (_kind n) of Map _ n_ref -> hasInvalidChild0 n_ref
                    _           -> return False

setKind :: IORef (Node a) -> Kind a -> IO ()
setKind n_ref k = modifyIORef' n_ref (\n -> n{ _kind = k })


-- | 'addParent' adds the parent node to the child node's parent list
-- Here the parent is added to the beginning of the parents list
-- The OCaml version takes the child_index for performance reason.
-- https://github.com/janestreet/incremental/blob/master/src/node.ml#L519
addParent :: IORef (Node a) -> IORef (Node a) -> IO ()
addParent child parent = do
  c <- readIORef child
  if (c^.pinfo.numPar) == 0
     then writeIORef child $ over pinfo (\p -> p{ _par0   = Just (PackedNode parent)
                                                , _numPar = 1 }) c
     else let old_pinfo = c^.pinfo
              new_pinfo =
                old_pinfo{ _par1AndBeyond = PackedNode parent : (old_pinfo^.par1AndBeyond)
                         , _numPar = old_pinfo^.numPar + 1 }
           in writeIORef child c{ _pinfo = new_pinfo }

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
  putStrLn $ "now n1 kind is " ++ show (_kind n1')
  case head $ n2'^.pinfo.par1AndBeyond of
    PackedNode r -> do
        n2_par <- readIORef r
        putStrLn $ "now n2_par kind is " ++ show (_kind n2_par)

---------------------------------- Helper --------------------------------------

