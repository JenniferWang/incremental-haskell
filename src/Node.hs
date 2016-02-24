module Node where

import Data.IORef
import Data.Unique
import Prelude hiding (id)
import Data.Maybe(fromJust, isJust, isNothing)

import Lens.Simple

import qualified Var as V
import qualified Kind as K
import Types
import Utils

---------------------------------- Node ---------------------------------------
newNode :: IO (NodeRef a)
newNode = do
  i <- newUnique
  let n_info = NodeInfo { _nid    = i
                        , _kind   = Invalid
                        , _value  = ValueInfo Nothing (-1) (-1)
                        , _numPar = 0
                        }
      n      = Node { _node   = n_info
                    , _parents = []
                    , _height = -1
                    , _handlers = HandlersInfo 0 False []
                    , _obsHead = Nothing
                    }
  newIORef n >>= return

-- Checks whether n is a valid node
-- it is if and only if its kind is valid
isValid :: Node a -> Bool
isValid n = case (n^.node.kind) of Invalid -> False
                                   _       -> True

isValid0 :: PackedNode -> IO Bool
isValid0 (PackedNode ref) = fmap isValid $ readIORef ref

-- | Check whether some IORef Node is the parent of current node
isParent :: PackedNode -> Node a -> Bool
isParent par_ref n = par_ref `elem` (n^.parents)

-- | 'iteriChildren' iterates all the child nodes
iteriChildren :: Node a -> (Index -> PackedNode -> b) -> [b]
iteriChildren n = K.iteriChildren (n^.node.kind)

maxNumChildren :: Node a -> Int
maxNumChildren n = K.maxNumChildren $ n^.node.kind

-- | 'getParent' returns the ref to the parent node
-- This is because later we might need to invalidate a node using this function
getParent :: Node a -> Index -> PackedNode
getParent n i = (n^.parents) !! i

-- | 'iteriParents' iterates all the parent nodes
iteriParents :: Node a -> (Index -> PackedNode -> b) -> [b]
iteriParents n g = map apply_g (zip [0..] (n^.parents))
  where apply_g (i, ref) = g i ref

hasChild :: Node a -> PackedNode -> Bool
hasChild n child = or $ iteriChildren n (\_ ref -> ref == child)

hasInvalidChild :: Node a -> IO Bool
hasInvalidChild n = or <$>
  (sequence $ iteriChildren n (\_ ref -> not <$> (isValid0 ref)))

hasInvalidChild0 :: NodeRef a-> IO Bool
hasInvalidChild0 ref = readIORef ref >>= hasInvalidChild

hasParent :: Node a -> PackedNode -> Bool
hasParent n parent = or $ iteriParents n (\_ ref -> ref == parent)

shouldBeInvalidated :: Node a -> IO Bool
shouldBeInvalidated n =
  case (n^.node.kind) of Map _ n_ref -> hasInvalidChild0 n_ref
                         _           -> return False

setKind :: NodeRef a -> Kind a -> IO ()
setKind ref k = modifyIORef' ref (\n -> n & node.kind.~ k)


-- | 'addParent' adds the parent node to the child node's parent list
-- Here the parent is added to the beginning of the parents list
-- The OCaml version takes the child_index for performance reason.
-- https://github.com/janestreet/incremental/blob/master/src/node.ml#L519
addParent :: NodeRef a -> NodeRef b -> IO ()
addParent child parent = do
  c <- readIORef child
  let c1 = c & node.numPar %~ (+1)
  writeIORef child (c1 & parents %~ (pack parent :))

removeParent :: NodeRef a -> NodeRef b -> IO ()
removeParent child parent = return ()

isNecessary :: Node a -> Bool
isNecessary n = (n^.node.numPar) > 0
             || isJust (n^.obsHead)
             || K.isFreeze (n^.node.kind)

setNodeValue :: NodeRef a -> Maybe a -> IO ()
setNodeValue ref new = modifyIORef' ref (\n -> n & node.value.v .~ new)

setChangedAt :: NodeRef a -> StabilizationNum -> IO ()
setChangedAt ref x = modifyIORef' ref (\n -> n & node.value.changedAt .~ x)

setRecomputedAt :: NodeRef a -> StabilizationNum -> IO ()
setRecomputedAt ref x = modifyIORef' ref (\n -> n & node.value.recomputedAt .~ x)

-- | 'valueExn' extracts the value from the node
valueExn :: Node a -> a
valueExn n
  | isNothing val = error "attempt to get value of an invalid node"
  | otherwise     = fromJust val
    where val = n^.node.value.v

test :: IO ()
test = do
  ref1 <- newNode
  ref2 <- newNode
  ref3 <- newNode
  addParent ref2 ref3
  addParent ref2 ref1
  return ()


---------------------------------- Helper --------------------------------------

