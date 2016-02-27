module State where
import Lens.Simple
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad (when, foldM)
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set

import Types
import Utils
import qualified Node as N
import qualified Recompute_Heap as RH

newNode :: StateIO (NodeRef a)
newNode = lift N.newNode

numStabilizes :: StateIO Int
numStabilizes = do s <- get; return $ s^.info.stbNum

amStabilizing :: StateIO Bool
amStabilizing = do
  s <- get
  case (s^.info.status) of
    NotStabilizing            -> return False
    StabilizePreviouslyRaised -> return False
    Stabilizing               -> return True
    RunningOnUpdateHandlers   -> return True

removeChildren :: NodeRef a -> StateIO ()
removeChildren par_ref = do
  par_node <- readIORefT (getRef par_ref)
  sequence_ $ N.iteriChildren par_node (\_ (PackedNode child_ref) ->
    do lift (N.removeParent child_ref par_ref)
       checkIfUnnecessary child_ref
    )

checkIfUnnecessary :: NodeRef a -> StateIO ()
checkIfUnnecessary nf@(Ref ref _) = do
  n <- readIORefT ref
  if (N.isNecessary n) then return ()
                       else becameUnnecessary nf

becameUnnecessary :: NodeRef a -> StateIO ()
becameUnnecessary nf@(Ref ref _)= do
  n <- readIORefT ref
  modify (\s -> s & info.debug.nodesBecame.unnecessary %~ (+ 1))
  when (n^.handlers.numOnUpdates > 0) (handleAfterStabilization nf)
  modifyIORefT ref (\n0 -> n0 & height .~ (-1))
  removeChildren nf
  -- TODO: kind = Unordered_array_fold / UnorderedArrayFold
  removeFromRecHeap (pack nf)

-- TODO: what does this function do?
handleAfterStabilization :: NodeRef a -> StateIO ()
handleAfterStabilization nf@(Ref ref _) = do
  n <- readIORefT ref
  if n^.handlers.isInHandleAfterStb
     then return ()
     else do modifyIORefT ref (\n0 -> n0 & handlers.isInHandleAfterStb .~ True)
             modify (\s -> s & handleAfterStb %~ (pack nf :))


-- | invalidate a valid node
-- https://github.com/janestreet/incremental/blob/master/src/state.ml#L357
invalidateNode :: Maybe PackedNode -> PackedNode -> StateIO Bool
invalidateNode _ pn@(PackedNode ref) = do
  n    <- readIORefT (getRef ref)
  flag <- (lift . N.shouldBeInvalidated) n
  if not (N.isValid n || flag)
     then return False
     else do
       when verbose (lift $ putStrLn "adding invalid node, invalidating parents")
       -- update node information
       env <- get
       lift (do N.setNodeValue ref Nothing
                N.setChangedAt ref (env^.info.stbNum)
                N.setRecomputedAt ref (env^.info.stbNum)
             )
       -- invalid node doesn't have children any more
       removeChildren ref
       -- TODO: update height
       -- TODO: scope?
       lift $ N.setKind ref Invalid
       -- remove invalid nodes from the recompute heap
       removeFromRecHeap pn
       return True

-- | use dfs to propagate Invalidity to parent nodes
propagateInvalidity :: NodeRef a -> StateIO ()
propagateInvalidity ref = dfsParentWithoutRepeat ref invalidateNode

adjustHeight :: Maybe PackedNode -> PackedNode -> StateIO Bool
adjustHeight Nothing _ = return True
adjustHeight (Just cpn) ppn@(PackedNode pref) = do
  ch <- lift (N.getHeightP cpn)
  ph <- lift (N.getHeightP ppn)
  if (ch < ph)
     then return False
     else do
       lift $ N.setHeight pref (ch + 1)
       return True
       -- TODO: update height in recompute heap
       -- TODO: add cap for maximum height

addParent :: NodeRef a -> NodeRef b -> StateIO ()
addParent child_ref par_ref = do
  child  <- readIORefT (getRef child_ref)
  parent <- readIORefT (getRef par_ref)
  let was_necessary = N.isNecessary child
  when (not $ N.isNecessary parent) (error "We only add necessary parent")
  lift $ N.addParent child_ref par_ref
  when (not was_necessary) (becameNecessary child_ref)
  -- if we add an invalid node to a parent => invalidate all the parents
  propagateInvalidity child_ref
  dfsParentWithRepeat child_ref adjustHeight
  -- TODO: when parent is not in recompute heap, and ... add parent to recompute heap

becameNecessary :: NodeRef a -> StateIO ()
becameNecessary nf@(Ref start _) = do
  n <- readIORefT start
  -- TODO: add fail conditions
  modify (\s -> s & info.debug.nodesBecame.necessary %~ (+ 1))
  when (n^.handlers.numOnUpdates > 0) (handleAfterStabilization nf)
  -- TODO: when stale, add to recompute heap

removeFromRecHeap :: PackedNode -> StateIO ()
removeFromRecHeap pn = modify (\s -> s & recHeap %~ (RH.remove pn))

---------------------------------- Helper --------------------------------------
readIORefT :: (IORef a) -> StateIO a
readIORefT = (lift . readIORef)

modifyIORefT :: (IORef a) -> (a -> a) -> StateIO ()
modifyIORefT ref g = lift $ modifyIORef ref g

-- | DFS parent nodes with a function 'check'. If 'check' returns True, then
-- continue searching for parent; otherwise, return
-- 'check' :: Maybe child -> current -> t IO Bool
dfsParentWithoutRepeat :: (MonadTrans t, Monad (t IO))
                       => NodeRef a
                       -> (Maybe PackedNode -> PackedNode -> t IO Bool)
                       -> t IO ()
dfsParentWithoutRepeat start check = go Nothing (pack start) Set.empty Set.empty
                                   >> return ()
  where
    -- go child curr path seen -> t IO (seen)
    go c x path seen
      | x `Set.member` path = error "Find a cycle"
      | x `Set.member` seen = return seen
      | otherwise = do
          result <- check c x
          if result then (rec x path seen) >>= return . (Set.insert x)
                    else return seen
    rec y@(PackedNode ref) path0 seen0 = do
      n <- (lift . readIORef) (getRef ref)
      let new_path = Set.insert y path0
      foldM (\s pn -> go (Just y) pn new_path (Set.insert y s)) seen0 (N.getParents n)

dfsParentWithRepeat :: (MonadTrans t, Monad (t IO))
                    => NodeRef a
                    -> (Maybe PackedNode -> PackedNode -> t IO Bool)
                    -> t IO ()
dfsParentWithRepeat start check = go Nothing (pack start) Set.empty
  where
    go c x path
      | x `Set.member` path = error "Find a cycle"
      | otherwise = do
          result <- check c x
          if result then (rec x path)
                    else return ()
    rec y@(PackedNode ref) path0 = do
      n <- (lift . readIORef) (getRef ref)
      let new_path = Set.insert y path0
      mapM_ (\pn -> go (Just y) pn new_path) (N.getParents n)


dfsParentWithRepeatP (PackedNode ref) = dfsParentWithRepeat ref

dfsParentWithoutRepeatP (PackedNode ref) = dfsParentWithoutRepeat ref

---------------------------------- Toy Test --------------------------------------
verbose :: Bool
verbose = True

-- | Create a graph for testing
testCreateGraph :: StateIO [PackedNode]
testCreateGraph = do
  n1 <- newNode
  n2 <- newNode
  n3 <- newNode
  n4 <- newNode
  addParent n1 n2
  addParent n1 n3
  addParent n2 n4
  addParent n3 n4
  return [pack n1, pack n2, pack n3, pack n4]

testPrintNodeInfo :: Maybe PackedNode -> PackedNode -> StateIO Bool
testPrintNodeInfo _ pn = do
  h <- (lift. N.getHeightP) pn
  (lift . putStrLn) $ "Reach node id = " ++ show pn ++ " with height = " ++ show h
  return True

-- | try to do dfs from a node and print the node id visited
testDfsParent :: StateIO ()
testDfsParent = do
  nodes <- testCreateGraph
  dfsParentWithoutRepeatP (nodes !! 0) testPrintNodeInfo

runTest :: StateIO a -> IO ()
runTest action = runStateT action initState >> return ()
