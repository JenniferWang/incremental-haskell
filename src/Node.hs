module Node where

import Data.IORef
import Data.Unique
import Prelude hiding (id)
import Data.Maybe(fromJust, isJust, isNothing)
import qualified Data.Set as Set

import Lens.Simple

import qualified Var as V
import qualified Kind as K
import Types
import Utils

---------------------------------- Node ---------------------------------------
newNode :: IO (NodeRef a)
newNode = do
  i <- newUnique
  ref  <- newIORef initNode
  return (Ref ref i)

-- Checks whether n is a valid node
-- it is if and only if its kind is valid
isValid :: Node a -> Bool
isValid n = case (n^.node.kind) of Invalid -> False
                                   _       -> True

isValidP :: PackedNode -> IO Bool
isValidP (PackedNode ref) = fmap isValid $ readIORef (getRef ref)

-- | Check whether some IORef Node is the parent of current node
isParent :: PackedNode -> Node a -> Bool
isParent par_ref n = par_ref `elem` (n^.parents)

-- | 'iteriChildren' iterates all the child nodes
iteriChildren :: Node a -> (Index -> PackedNode -> b) -> [b]
iteriChildren n = K.iteriChildren (n^.node.kind)

maxNumChildren :: Node a -> Int
maxNumChildren n = K.maxNumChildren $ n^.node.kind

getParents :: Node a -> [PackedNode]
getParents n = iteriParents n (\_ p -> p)

-- | 'iteriParents' iterates all the parent nodes
iteriParents :: Node a -> (Index -> PackedNode -> b) -> [b]
iteriParents n g = map apply_g (zip [0..] (Set.toList $ n^.parents))
  where apply_g (i, ref) = g i ref

hasChild :: Node a -> PackedNode -> Bool
hasChild n child = or $ iteriChildren n (\_ ref -> ref == child)

hasInvalidChild :: Node a -> IO Bool
hasInvalidChild n = or <$>
  (sequence $ iteriChildren n (\_ ref -> not <$> (isValidP ref)))

hasInvalidChild0 :: NodeRef a-> IO Bool
hasInvalidChild0 ref = readIORef (getRef ref) >>= hasInvalidChild

hasParent :: Node a -> PackedNode -> Bool
hasParent n parent = or $ iteriParents n (\_ ref -> ref == parent)

shouldBeInvalidated :: Node a -> IO Bool
shouldBeInvalidated n =
  case (n^.node.kind) of Map _ n_ref -> hasInvalidChild0 n_ref
                         _           -> return False

setKind :: NodeRef a -> Kind a -> IO ()
setKind ref k = modifyIORef' (getRef ref) (\n -> n & node.kind.~ k)


-- | 'addParent' adds the parent node to the child node's parent list
-- Here the parent is added to the beginning of the parents list
-- The OCaml version takes the child_index for performance reason.
-- https://github.com/janestreet/incremental/blob/master/src/node.ml#L519
addParent :: NodeRef a -> NodeRef b -> IO ()
addParent (Ref cref _) parent = do
  c <- readIORef cref
  let c1 = c & node.numPar %~ (+ 1)
  writeIORef cref (c1 & parents %~ (Set.insert $ pack parent))

-- | 'removeParent' does not check whether [parent] is a true parent of [child]
removeParent :: NodeRef a -> NodeRef b -> IO ()
removeParent (Ref cref _) parent = do
  c <- readIORef cref
  let c1 = c & node.numPar %~ ((-) 1)
  writeIORef cref (c1 & parents %~ (Set.delete $ pack parent))

isNecessary :: Node a -> Bool
isNecessary n = (n^.node.numPar) > 0
             || isJust (n^.obsHead)
             || K.isFreeze (n^.node.kind)

setNodeValue :: NodeRef a -> Maybe a -> IO ()
setNodeValue ref v0 = modifyIORef' (getRef ref)
                                    (\n -> n & node.value.v .~ v0)

setChangedAt :: NodeRef a -> StabilizationNum -> IO ()
setChangedAt ref x = modifyIORef' (getRef ref)
                                  (\n -> n & node.value.changedAt .~ x)

setRecomputedAt :: NodeRef a -> StabilizationNum -> IO ()
setRecomputedAt ref x = modifyIORef' (getRef ref)
                                     (\n -> n & node.value.recomputedAt .~ x)

getHeight :: NodeRef a -> IO Height
getHeight ref = readIORef (getRef ref) >>= \n -> return (n^.height)

getHeightP :: PackedNode -> IO Height
getHeightP (PackedNode noderef) = getHeight noderef

setHeight :: NodeRef a -> Height -> IO ()
setHeight ref h = modifyIORef (getRef ref) (\n -> n & height .~ h)

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

