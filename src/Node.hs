module Node where

import Data.IORef
import Lens.Simple
import qualified Var as V
import qualified Kind as K
import Types
import Prelude hiding (id)
import Data.Maybe(fromJust, isJust, isNothing)

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
           n_info = NodeInfo { nid             = i
                             , _kind           = Invalid
                             , _forceNecessary = False
                             , _obsHead        = Nothing
                             , _value          = ValueInfo Nothing (-1) (-1)
                             }
           h_info = HeapInfo { _rec = RecHeapInfo (-1) Nothing Nothing
                             , _adj = AdjHeapInfo (-1) Nothing
                             }

       return $ Node { _node   = n_info
                     , _par    = p_info
                     , _height = -1
                     , _heap   = h_info
                     , _handlers = HandlersInfo 0 False []
                     }

-- Checks whether n is a valid node
-- it is if and only if its kind is valid
isValid :: Node a -> Bool
isValid n = case (n^.node.kind) of Invalid -> False
                                   _       -> True

isValid0 :: PackedNode -> IO Bool
isValid0 (PackedNode ref) = fmap isValid $ readIORef ref

-- | Check whether some IORef Node is the parent of current node
isParent :: PackedNode -> Node a -> Bool
isParent par_ref n
  | n^.par.numPar == 0 = False
  | otherwise = par_ref == fromJust (n^.par.par0)
             || par_ref `elem` (n^.par.par1AndBeyond)

-- | 'iteriChildren' iterates all the child nodes
iteriChildren :: Node a -> (Index -> PackedNode -> b) -> [b]
iteriChildren n g
  | n^.par.numPar == 0 = []
  | otherwise = K.iteriChildren (n^.node.kind) g

maxNumChildren :: Node a -> Int
maxNumChildren n = K.maxNumChildren $ n^.node.kind

-- | 'getParent' returns the ref to the parent node
-- This is because later we might need to invalidate a node using this function
getParent :: Node a -> Index -> PackedNode
getParent n i =
  if i == 0 then (fromJust $ n^.par.par0)
            else (n^.par.par1AndBeyond) !! (i - 1)

-- | 'iteriParents' iterates all the parent nodes
iteriParents :: Node a -> (Index -> PackedNode -> b) -> [b]
iteriParents n g
  | n^.par.numPar == 0 = []
  | otherwise            = g0 : rest
    where g0               = g 0 (fromJust $ n^.par.par0)
          apply_g (i, ref) = g i ref
          rest             = map apply_g (zip [1..] (n^.par.par1AndBeyond))

hasChild :: Node a -> PackedNode -> Bool
hasChild n child = or $ iteriChildren n (\_ ref -> ref == child)

hasInvalidChild :: Node a -> IO Bool
hasInvalidChild n = or <$>
  (sequence $ iteriChildren n (\_ ref -> not <$> (isValid0 ref)))

hasInvalidChild0 :: IORef (Node a) -> IO Bool
hasInvalidChild0 ref = readIORef ref >>= hasInvalidChild

hasParent :: Node a -> PackedNode -> Bool
hasParent n parent = or $ iteriParents n (\_ ref -> ref == parent)

shouldBeInvalidated :: Node a -> IO Bool
shouldBeInvalidated n =
  case (n^.node.kind) of Map _ n_ref -> hasInvalidChild0 n_ref
                         _           -> return False

setKind :: IORef (Node a) -> Kind a -> IO ()
setKind ref k = modifyIORef' ref (\n -> n & node.kind.~ k)


-- | 'addParent' adds the parent node to the child node's parent list
-- Here the parent is added to the beginning of the parents list
-- The OCaml version takes the child_index for performance reason.
-- https://github.com/janestreet/incremental/blob/master/src/node.ml#L519
addParent :: IORef (Node a) -> IORef (Node b) -> IO ()
addParent child parent = do
  c <- readIORef child
  let c1 = c & par.numPar %~ (+1)
  if (c^.par.numPar) == 0
    then writeIORef child (c1 & par.par0 .~ Just (pack parent))
    else writeIORef child (c1 & par.par1AndBeyond %~ (pack parent :))

removeParent :: IORef (Node a) -> IORef (Node b) -> IO ()
removeParent child parent = return ()

pack :: IORef (Node a) -> PackedNode
pack = PackedNode

isNecessary :: Node a -> Bool
isNecessary n = (n^.par.numPar) > 0
             || isJust (n^.node.obsHead)
             || K.isFreeze (n^.node.kind)
             || n^.node.forceNecessary

-- | 'valueExn' extracts the value from the node
valueExn :: Node a -> a
valueExn n
  | isNothing val = error "attempt to get value of an invalid node"
  | otherwise     = fromJust val
    where val = n^.node.value.v

isInRecomputeHeap :: Node a -> Bool
isInRecomputeHeap n = (n^.heap.rec.heightInRecHeap) >= 0

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
  putStrLn $ "now n1 kind is " ++ show (n1'^.node.kind)
  case head $ n2'^.par.par1AndBeyond of
    PackedNode r -> do
        n2_par <- readIORef r
        putStrLn $ "now n2_par kind is " ++ show (n2_par^.node.kind)

---------------------------------- Helper --------------------------------------

