module Node where

import Data.IORef
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift)
import Data.Unique
import Prelude hiding (id)
import Data.Maybe(fromJust, isNothing)
import qualified Data.Set as Set

import Lens.Simple

import qualified Var              as V
import qualified Kind             as K
import qualified StabilizationNum as Stb
import qualified Scope            as Scope
import Types
import Utils

---------------------------------- Node ---------------------------------------
create :: (Eq a) => Scope -> Kind a -> StateIO (NodeRef a)
create create_in k = do
  i   <- lift $ newUnique
  ref <- lift $ newIORef (initNode{ _kind = k, _createdIn = create_in })
  let node_ref = Ref ref i
  Scope.addNode create_in node_ref
  return node_ref

-- Checks whether n is a valid node
-- it is if and only if its kind is valid
isValid :: (Eq a) => Node a -> Bool
isValid n = case (n^.kind) of Invalid -> False
                              _       -> True

-- | pre: [par_ref] is a parent of [child]
edgeIsStale :: (Eq a) => Node a -> PackedNode -> StateIO Bool
edgeIsStale child (PackedNode par_ref) = do
  parent <- readNodeRef par_ref
  let p_recomp = parent^.value.recomputedAt
  return $ (Stb.isNone p_recomp) ||
    (child^.value.changedAt) > (parent^.value.recomputedAt)

isStaleWithRespectToAChild :: (Eq a) => Node a -> StateIO Bool
isStaleWithRespectToAChild node = and <$>
  sequence (iteriChildren node (\_ pn -> edgeIsStale node pn))

-- | Check whether some IORef Node is the parent of current node
isParent :: (Eq a) => PackedNode -> Node a -> Bool
isParent par_ref n = par_ref `elem` (n^.edges.parents)

-- | 'iteriChildren' iterates all the child nodes
iteriChildren :: (Eq a) => Node a -> (Index -> PackedNode -> b) -> [b]
iteriChildren n = K.iteriChildren (n^.kind)

maxNumChildren :: Node a -> Int
maxNumChildren n = K.maxNumChildren $ n^.kind

getParents :: Node a -> [PackedNode]
getParents n = iteriParents n (\_ p -> p)

-- | 'iteriParents' iterates all the parent nodes
iteriParents :: Node a -> (Index -> PackedNode -> b) -> [b]
iteriParents n g = map apply_g (zip [0..] (Set.toList $ n^.edges.parents))
  where apply_g (i, ref) = g i ref

hasChild :: (Eq a) => Node a -> PackedNode -> Bool
hasChild n child = or $ iteriChildren n (\_ ref -> ref == child)

hasInvalidChild :: (Eq a) => Node a -> StateIO Bool
hasInvalidChild n = or <$>
  (sequence $ iteriChildren n (\_ ref -> not <$> (go ref)))
  where go (PackedNode ref) = readNodeRef ref >>= return . isValid

hasParent :: (Eq a) => Node a -> PackedNode -> Bool
hasParent n parent = or $ iteriParents n (\_ ref -> ref == parent)

shouldBeInvalidated :: (Eq a) => Node a -> StateIO Bool
shouldBeInvalidated n =
  case (n^.kind) of ArrayFold _ _ _ -> hasInvalidChild n
                    Map  _ _        -> hasInvalidChild n
                    Map2 _ _ _      -> hasInvalidChild n
                    Map3 _ _ _ _    -> hasInvalidChild n
                    Map4 _ _ _ _ _  -> hasInvalidChild n
                    _               -> return False

setKind :: (Eq a) => NodeRef a -> Kind a -> StateIO ()
setKind ref k = modifyNodeRef ref (\n -> n{_kind = k})

-- | 'addParent' adds the parent node to the child node's parent list
-- Here the parent is added to the beginning of the parents list
-- The OCaml version takes the child_index for performance reason.
-- https://github.com/janestreet/incremental/blob/master/src/node.ml#L519
addParent :: (Eq a, Eq b) => NodeRef a -> NodeRef b -> StateIO ()
addParent ref parent =
  modifyNodeRef ref (\c -> c & edges.parents %~ (Set.insert $ pack parent))

-- | 'removeParent' does not check whether [parent] is a true parent of [child]
removeParent :: (Eq a, Eq b) => NodeRef a -> NodeRef b -> IO ()
removeParent (Ref cref _) parent = do
  c <- readIORef cref
  writeIORef cref (c & edges.parents %~ (Set.delete $ pack parent))

numParents :: (Eq a) => Node a -> Int
numParents n = Set.size $ n^.edges.parents

isNecessary :: (Eq a) => Node a -> Bool
isNecessary n = (numParents n) > 0
             || (not $ Set.null (n^.edges.obsOnNode))
             || K.isFreeze (n^.kind)

-- | 'valueExn' extracts the value from the node
valueExn :: (Eq a) => Node a -> a
valueExn n
  | isNothing val = error "Node.valueExn: attempt to get value of an invalid node"
  | otherwise     = fromJust val
    where val = n^.value.v

setNodeValue :: Eq a => NodeRef a -> Maybe a -> StateIO ()
setNodeValue (Ref ref _) v0 = modifyIORefT ref
                                (\n -> n & value.v .~ v0)

getNodeValue :: Eq a => NodeRef a -> StateIO (Maybe a)
getNodeValue (Ref ref _) = readIORefT ref >>= \n -> return (n^.value.v)

updateChangedAt :: Eq a => NodeRef a -> StateIO ()
updateChangedAt (Ref ref _) = do
  env <- get
  modifyIORefT ref (\n -> n & value.changedAt .~ (env^.info.stbNum))

updateRecomputedAt :: Eq a => NodeRef a -> StateIO ()
updateRecomputedAt (Ref ref _) = do
  env <- get
  modifyIORefT ref (\n -> n & value.recomputedAt .~ (env^.info.stbNum))

removeObs :: Eq a => NodeRef a -> ObsID -> StateIO ()
removeObs (Ref ref _) i = do
  modifyIORefT ref (\n -> n & edges.obsOnNode %~ (Set.delete i))

getParentsP :: PackedNode -> StateIO [PackedNode]
getParentsP (PackedNode noderef) = do
  n <- readIORefT (getRef noderef)
  return $ getParents n

isStaleP :: (Eq a) => NodeRef a -> StateIO Bool
isStaleP ref = do
  node <- readNodeRef ref
  let rcp = node^.value.recomputedAt
  case node^.kind of
    ArrayFold _ _ _     -> isStaleWithRespectToAChild node
    Uninitialized       -> error "[Node.isStale] The node is uninitialized"
    Invalid             -> return False
    Variable _ _ set_at -> return $ set_at > rcp
    -- A const node is stale only at initialization.
    Const _             -> return $ Stb.isNone rcp
    Map _ _             -> isStaleWithRespectToAChild node
    Map2 _ _ _          -> isStaleWithRespectToAChild node
    Map3 _ _ _ _        -> isStaleWithRespectToAChild node
    Map4 _ _ _ _ _      -> isStaleWithRespectToAChild node
    Bind _ _ _ _        -> isStaleWithRespectToAChild node
    _                   -> return $ Stb.isNone rcp

