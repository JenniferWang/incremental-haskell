module Node where

import Data.IORef
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
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
createNode :: (Eq a) => IO (NodeRef a)
createNode = do
  i <- newUnique
  ref  <- newIORef initNode
  return (Ref ref i)

-- Checks whether n is a valid node
-- it is if and only if its kind is valid
isValid :: (Eq a) => Node a -> Bool
isValid n = case (n^.node.kind) of Invalid -> False
                                   _       -> True

isValidP :: PackedNode -> IO Bool
isValidP (PackedNode ref) = fmap isValid $ readIORef (getRef ref)

-- | Check whether some IORef Node is the parent of current node
isParent :: (Eq a) => PackedNode -> Node a -> Bool
isParent par_ref n = par_ref `elem` (n^.parents)

-- | 'iteriChildren' iterates all the child nodes
iteriChildren :: (Eq a) => Node a -> (Index -> PackedNode -> b) -> [b]
iteriChildren n = K.iteriChildren (n^.node.kind)

maxNumChildren :: Node a -> Int
maxNumChildren n = K.maxNumChildren $ n^.node.kind

getParents :: Node a -> [PackedNode]
getParents n = iteriParents n (\_ p -> p)

-- | 'iteriParents' iterates all the parent nodes
iteriParents :: Node a -> (Index -> PackedNode -> b) -> [b]
iteriParents n g = map apply_g (zip [0..] (Set.toList $ n^.parents))
  where apply_g (i, ref) = g i ref

hasChild :: (Eq a) => Node a -> PackedNode -> Bool
hasChild n child = or $ iteriChildren n (\_ ref -> ref == child)

hasInvalidChild :: (Eq a) => Node a -> IO Bool
hasInvalidChild n = or <$>
  (sequence $ iteriChildren n (\_ ref -> not <$> (isValidP ref)))

hasInvalidChild0 :: (Eq a) => NodeRef a-> IO Bool
hasInvalidChild0 ref = readIORef (getRef ref) >>= hasInvalidChild

hasParent :: (Eq a) => Node a -> PackedNode -> Bool
hasParent n parent = or $ iteriParents n (\_ ref -> ref == parent)

shouldBeInvalidated :: (Eq a) => Node a -> IO Bool
shouldBeInvalidated n =
  case (n^.node.kind) of Map _ n_ref -> hasInvalidChild0 n_ref
                         _           -> return False

setKind :: (Eq a) => NodeRef a -> Kind a -> IO ()
setKind ref k = modifyIORef' (getRef ref) (\n -> n & node.kind.~ k)


-- | 'addParent' adds the parent node to the child node's parent list
-- Here the parent is added to the beginning of the parents list
-- The OCaml version takes the child_index for performance reason.
-- https://github.com/janestreet/incremental/blob/master/src/node.ml#L519
addParent :: (Eq a, Eq b) => NodeRef a -> NodeRef b -> IO ()
addParent (Ref cref _) parent = do
  c <- readIORef cref
  let c1 = c & node.numPar %~ (+ 1)
  writeIORef cref (c1 & parents %~ (Set.insert $ pack parent))

-- | 'removeParent' does not check whether [parent] is a true parent of [child]
removeParent :: (Eq a, Eq b) => NodeRef a -> NodeRef b -> IO ()
removeParent (Ref cref _) parent = do
  c <- readIORef cref
  let c1 = c & node.numPar %~ ((-) 1)
  writeIORef cref (c1 & parents %~ (Set.delete $ pack parent))

isNecessary :: (Eq a) => Node a -> Bool
isNecessary n = (n^.node.numPar) > 0
             || (not $ Set.null (n^.obsOnNode))
             || K.isFreeze (n^.node.kind)


-- | 'valueExn' extracts the value from the node
valueExn :: (Eq a) => Node a -> a
valueExn n
  | isNothing val = error "attempt to get value of an invalid node"
  | otherwise     = fromJust val
    where val = n^.node.value.v

------------------------------ StateIO Monad -------------------------------
setNodeValue :: Eq a => NodeRef a -> Maybe a -> StateIO ()
setNodeValue (Ref ref _) v0 = modifyIORefT ref
                                (\n -> n & node.value.v .~ v0)

getNodeValue :: Eq a => NodeRef a -> StateIO (Maybe a)
getNodeValue (Ref ref _) = readIORefT ref >>= \n -> return (n^.node.value.v)

updateChangedAt :: Eq a => NodeRef a -> StateIO ()
updateChangedAt (Ref ref _) = do
  env <- get
  modifyIORefT ref (\n -> n & node.value.changedAt .~ (env^.info.stbNum))

updateRecomputedAt :: Eq a => NodeRef a -> StateIO ()
updateRecomputedAt (Ref ref _) = do
  env <- get
  modifyIORefT ref (\n -> n & node.value.recomputedAt .~ (env^.info.stbNum))

getHeight :: Eq a => NodeRef a -> StateIO Height
getHeight (Ref ref _) = readIORefT ref >>= \n -> return (n^.height)

getHeightP :: PackedNode -> StateIO Height
getHeightP (PackedNode noderef) = getHeight noderef

setHeight :: Eq a => NodeRef a -> Height -> StateIO ()
setHeight (Ref ref _) h = modifyIORefT ref (\n -> n & height .~ h)

removeObs :: Eq a => NodeRef a -> ObsID -> StateIO ()
removeObs (Ref ref _) i = do
  modifyIORefT ref (\n -> n & obsOnNode %~ (Set.delete i))

test :: IO ()
test = do
  ref1 <- createNode :: IO (NodeRef Int)
  ref2 <- createNode :: IO (NodeRef Int)
  ref3 <- createNode :: IO (NodeRef Int)
  addParent ref2 ref3
  addParent ref2 ref1
  return ()


---------------------------------- Helper --------------------------------------

