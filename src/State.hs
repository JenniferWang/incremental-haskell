module State where
import Lens.Simple
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad (when, foldM)
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set

import Types
import Utils
import qualified Node as N

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

setHeight :: NodeRef a -> Height -> StateIO ()
setHeight = undefined

setMaxHeightAllowed :: Height -> StateIO ()
setMaxHeightAllowed h = do
  s <- get
  return ()
  -- TODO: change adjust_height_heap
  -- TODO: change recompute_heap

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
  -- TODO: when (N.isInRecomputeHeap n) (remove from recompute heap)

handleAfterStabilization :: NodeRef a -> StateIO ()
handleAfterStabilization nf@(Ref ref _) = do
  n <- readIORefT ref
  if n^.handlers.isInHandleAfterStb
     then return ()
     else do (modifyIORefT ref (\n0 -> n0 & handlers.isInHandleAfterStb .~ True))
             modify (\s -> s & handleAfterStb %~ (pack nf :))

-- | An invalid node is node whose kind is [Invalid]
-- A node's kind is set to [Invalid] when the lhs of its scope changes,
-- or one if its children propagate the invalidity upward
-- Invalidating a node disconnects it from its children, which means:
--   1. an invalid node cannot end up on the scheduler
--   2. an invalid node doesn't make its children necessary anymore.
invalidateNode :: PackedNode -> StateIO Bool
invalidateNode = undefined
-- invalidateNode (PackedNode ref) = do
  -- n <- readIORefT (getRef ref)
  -- if not (N.isValid n)
  --    then return True
  --    else
-- invalidateNode ref = do
--   n <- readIORef ref
--   when (N.isValid n) do
--     n^.handlers.numOnUpdates > 0

invalidateValidNode :: NodeRef a -> StateIO ()
invalidateValidNode nf@(Ref ref _) = do
  env <- get
  n   <- readIORefT ref
  when (n^.handlers.numOnUpdates > 0) (handleAfterStabilization nf)
  lift ( do N.setNodeValue nf Nothing
            N.setChangedAt nf (env^.info.stbNum)
            N.setRecomputedAt nf (env^.info.stbNum)
       )

-- | use dfs to propagate Invalidity to parent nodes
propagateInvalidity :: NodeRef a -> StateIO ()
propagateInvalidity ref = dfsParent ref invalidateNode

addParent :: NodeRef a -> NodeRef b -> StateIO ()
addParent child_ref par_ref = do
  lift $ N.addParent child_ref par_ref
  -- propagate_invalidity
  -- became_necessary
  -- adjust height

-- becameNecessary :: NodeRef a -> StateIO ()
-- becameNecessary nf@(Ref start _) = do
--   n <- readIORefT start
--   modify (\s -> s & info.debug.nodesBecame.necessary %~ (+ 1))
--   when (n^.handlers.numOnUpdates > 0) (handleAfterStabilization nf)

---------------------------------- Helper --------------------------------------
readIORefT :: (IORef a) -> StateIO a
readIORefT = (lift . readIORef)

modifyIORefT :: (IORef a) -> (a -> a) -> StateIO ()
modifyIORefT ref g = lift $ modifyIORef ref g

-- | DFS parent nodes with a function 'check'. If 'check' returns True, then
-- continue searching for parent; otherwise, return
dfsParent :: (MonadTrans t, Monad (t IO)) =>
             NodeRef a -> (PackedNode -> t IO Bool) -> t IO ()
dfsParent start check = go (pack start) Set.empty Set.empty >> return ()
  where
    -- go node path seen -> IO (seen)
    go x path seen
      | x `Set.member` path = error "Find a cycle"
      | x `Set.member` seen = return seen
      | otherwise = do
          result <- check x
          if result  then (rec x path seen) >>= return . (Set.insert x)
                     else return seen
    rec y@(PackedNode ref) path0 seen0 = do
      n <- (lift . readIORef) (getRef ref)
      let new_path = Set.insert y path0
      foldM (\s pn -> go pn new_path (Set.insert y s)) seen0 (N.getParents n)

dfsParentP :: (MonadTrans t, Monad (t IO)) =>
              PackedNode -> (PackedNode -> t IO Bool) -> t IO ()
dfsParentP (PackedNode nf) check = dfsParent nf check


---------------------------------- Toy Test --------------------------------------
-- | Create a graph for testing
testCreateGraph :: IO [PackedNode]
testCreateGraph = do
  n1 <- N.newNode
  n2 <- N.newNode
  n3 <- N.newNode
  n4 <- N.newNode
  N.addParent n1 n2
  N.addParent n1 n3
  N.addParent n2 n4
  N.addParent n3 n4
  return [pack n1, pack n2, pack n3, pack n4]

testPrintNode :: PackedNode -> StateIO Bool
testPrintNode pn = do
  (lift . putStrLn) $ "Reach node " ++ show pn
  return True

-- | try to do dfs from a node and print the node id visited
testDfsParent :: IO ()
testDfsParent = do
  nodes <- testCreateGraph
  mapM_ (\n -> putStrLn $ "The graph has node id = " ++ show n) nodes
  runStateT (dfsParentP (nodes !! 0) testPrintNode) initState
  return ()

