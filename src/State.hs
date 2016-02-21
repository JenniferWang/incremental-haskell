module State where
import Lens.Simple
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad (when)
import Data.IORef

import Types
import qualified Node as N

numStabilizes :: StateIO Int
numStabilizes = do s <- get; return $ s^.stb.stbNum

amStabilizing :: StateIO Bool
amStabilizing = do
  s <- get
  case (s^.status) of
    NotStabilizing            -> return False
    StabilizePreviouslyRaised -> return False
    Stabilizing               -> return True
    RunningOnUpdateHandlers   -> return True

setHeight :: IORef (Node a) -> Height -> StateIO ()
setHeight = undefined

setMaxHeightAllowed :: Height -> StateIO ()
setMaxHeightAllowed h = do
  s <- get
  return ()
  -- TODO: change adjust_height_heap
  -- TODO: change recompute_heap

removeChildren :: IORef (Node a) -> StateIO ()
removeChildren par_ref = do
  par_node         <- readIORefT par_ref
  sequence_ $ N.iteriChildren par_node (\i (PackedNode child_ref) ->
    do lift (N.removeParent child_ref par_ref)
       checkIfUnnecessary child_ref
    )

checkIfUnnecessary :: IORef (Node a) -> StateIO ()
checkIfUnnecessary ref = do
  n <- readIORefT ref
  if (N.isNecessary n) then return ()
                       else becameUnnecessary ref

becameUnnecessary :: IORef (Node a) -> StateIO ()
becameUnnecessary ref = do
  n <- readIORefT ref
  modify (\s -> s & num.nodesBecame.unnecessary %~ (+ 1))
  when (n^.handlers.numOnUpdates > 0) (handleAfterStabilization ref)
  modifyIORefT ref (\n0 -> n0 & height .~ (-1))
  removeChildren ref
  -- TODO: kind = Unordered_array_fold / UnorderedArrayFold
  -- TODO: when (N.isInRecomputeHeap n) (remove from recompute heap)

handleAfterStabilization :: IORef (Node a) -> StateIO ()
handleAfterStabilization ref = do
  n <- readIORefT ref
  if n^.handlers.isInHandleAfterStb
     then return ()
     else do (modifyIORefT ref (\n0 -> n0 & handlers.isInHandleAfterStb .~ True))
             -- TODO push to state.handle_after_stabilization

---------------------------------- Helper --------------------------------------
readIORefT :: (IORef a) -> StateIO a
readIORefT = (lift . readIORef)

modifyIORefT :: (IORef a) -> (a -> a) -> StateIO ()
modifyIORefT ref g = lift $ modifyIORef ref g
