module State where
import Lens.Simple
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad (when)
import Data.IORef

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
  par_node <- readIORefT par_ref
  sequence_ $ N.iteriChildren par_node (\_ (PackedNode child_ref) ->
    do lift (N.removeParent child_ref par_ref)
       checkIfUnnecessary child_ref
    )

checkIfUnnecessary :: NodeRef a -> StateIO ()
checkIfUnnecessary ref = do
  n <- readIORefT ref
  if (N.isNecessary n) then return ()
                       else becameUnnecessary ref

becameUnnecessary :: NodeRef a -> StateIO ()
becameUnnecessary ref = do
  n <- readIORefT ref
  modify (\s -> s & info.debug.nodesBecame.unnecessary %~ (+ 1))
  when (n^.handlers.numOnUpdates > 0) (handleAfterStabilization ref)
  modifyIORefT ref (\n0 -> n0 & height .~ (-1))
  removeChildren ref
  -- TODO: kind = Unordered_array_fold / UnorderedArrayFold
  -- TODO: when (N.isInRecomputeHeap n) (remove from recompute heap)

handleAfterStabilization :: NodeRef a -> StateIO ()
handleAfterStabilization ref = do
  n <- readIORefT ref
  if n^.handlers.isInHandleAfterStb
     then return ()
     else do (modifyIORefT ref (\n0 -> n0 & handlers.isInHandleAfterStb .~ True))
             modify (\s -> s & handleAfterStb %~ (pack ref :))

-- | An invalid node is node whose kind is [Invalid]
-- A node's kind is set to [Invalid] when the lhs of its scope changes,
-- or one if its children propagate the invalidity upward
-- Invalidating a node disconnects it from its children, which means:
--   1. an invalid node cannot end up on the scheduler
--   2. an invalid node doesn't make its children necessary anymore.
invalidateNode :: NodeRef a -> StateIO ()
invalidateNode = undefined
-- invalidateNode ref = do
--   n <- readIORef ref
--   when (N.isValid n) do
--     n^.handlers.numOnUpdates > 0

invalidateValidNode :: NodeRef a -> StateIO ()
invalidateValidNode ref = do
  env <- get
  n   <- readIORefT ref
  when (n^.handlers.numOnUpdates > 0) (handleAfterStabilization ref)
  lift ( do N.setNodeValue ref Nothing
            N.setChangedAt ref (env^.info.stbNum)
            N.setRecomputedAt ref (env^.info.stbNum)
       )


addParent :: NodeRef a -> NodeRef b -> StateIO ()
addParent child_ref par_ref = do
  lift $ N.addParent child_ref par_ref
  -- adjust height


---------------------------------- Helper --------------------------------------
readIORefT :: (IORef a) -> StateIO a
readIORefT = (lift . readIORef)

modifyIORefT :: (IORef a) -> (a -> a) -> StateIO ()
modifyIORefT ref g = lift $ modifyIORef ref g
