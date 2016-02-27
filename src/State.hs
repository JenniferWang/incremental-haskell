module State where
import Lens.Simple
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad (when, foldM)
import Data.IORef
import Data.Set (Set)
import Data.Maybe(isNothing, fromJust)
import qualified Data.Set as Set

import Types
import Utils
import qualified Node as N
import qualified Recompute_Heap as RH

newNode :: Eq a => StateIO (NodeRef a)
newNode = lift N.newNode

getStbNum:: StateIO Int
getStbNum = do s <- get; return $ s^.info.stbNum

amStabilizing :: StateIO Bool
amStabilizing = do
  s <- get
  case (s^.info.status) of
    NotStabilizing            -> return False
    StabilizePreviouslyRaised -> return False
    Stabilizing               -> return True
    RunningOnUpdateHandlers   -> return True

removeChildren :: Eq a => NodeRef a -> StateIO ()
removeChildren par_ref = do
  par_node <- readIORefT (getRef par_ref)
  sequence_ $ N.iteriChildren par_node (\_ (PackedNode child_ref) ->
    do lift (N.removeParent child_ref par_ref)
       checkIfUnnecessary child_ref
    )

checkIfUnnecessary :: Eq a => NodeRef a -> StateIO ()
checkIfUnnecessary nf@(Ref ref _) = do
  n <- readIORefT ref
  if (N.isNecessary n) then return ()
                       else becameUnnecessary nf

becameUnnecessary :: Eq a => NodeRef a -> StateIO ()
becameUnnecessary nf@(Ref ref _)= do
  n <- readIORefT ref
  modify (\s -> s & info.debug.nodesBecame.unnecessary %~ (+ 1))
  when (n^.handlers.numOnUpdates > 0) (handleAfterStabilization nf)
  modifyIORefT ref (\n0 -> n0 & height .~ (-1))
  removeChildren nf
  -- TODO: kind = Unordered_array_fold / UnorderedArrayFold
  removeFromRecHeap (pack nf)

-- TODO: what does this function do?
handleAfterStabilization :: Eq a => NodeRef a -> StateIO ()
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
       setNodeValue ref Nothing
       updateChangedAt ref
       updateRecomputedAt ref
       -- invalid node doesn't have children any more
       removeChildren ref
       -- TODO: update height
       -- TODO: scope?
       lift $ N.setKind ref Invalid
       -- remove invalid nodes from the recompute heap
       removeFromRecHeap pn
       return True

-- | use dfs to propagate Invalidity to parent nodes
propagateInvalidity :: Eq a => NodeRef a -> StateIO ()
propagateInvalidity ref = dfsParentWithoutRepeat ref invalidateNode

adjustHeight :: Maybe PackedNode -> PackedNode -> StateIO Bool
adjustHeight Nothing _ = return True
adjustHeight (Just cpn) ppn@(PackedNode pref) = do
  ch <- getHeightP cpn
  ph <- getHeightP ppn
  if (ch < ph)
     then return False
     else do
       setHeight pref (ch + 1)
       return True
       -- TODO: update height in recompute heap
       -- TODO: add cap for maximum height

addParent :: (Eq a, Eq b) => NodeRef a -> NodeRef b -> StateIO ()
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

becameNecessary :: Eq a => NodeRef a -> StateIO ()
becameNecessary nf@(Ref start _) = do
  n <- readIORefT start
  -- TODO: add fail conditions
  modify (\s -> s & info.debug.nodesBecame.necessary %~ (+ 1))
  when (n^.handlers.numOnUpdates > 0) (handleAfterStabilization nf)
  -- TODO: when stale, add to recompute heap

removeFromRecHeap :: PackedNode -> StateIO ()
removeFromRecHeap pn = modify (\s -> s & recHeap %~ (RH.remove pn))

changeChild :: (Eq a, Eq b) => NodeRef a -> NodeRef b -> NodeRef b -> StateIO ()
changeChild parent old_child new_child = do
  if old_child == new_child
     then return ()
     else do
       lift $ N.removeParent old_child parent
       -- TODO: force necessary?
       addParent new_child parent
       checkIfUnnecessary old_child

recompute :: Eq a => NodeRef a -> StateIO ()
recompute curr = do
  modify (\s -> s & info.debug.nodesRecomputed.byDefault %~ (+ 1))
  updateRecomputedAt curr
  n0 <- readIORefT (getRef curr)
  case n0^.node.kind of
    Const x                -> maybeChangeValue curr x
    Freeze _ cref f        -> do
      cv <- valueExn cref
      when (f cv) $ do removeChildren curr
                       lift $ N.setKind curr (Const cv)
                       if (N.isNecessary n0)
                          then setHeight curr 0
                          else becameUnnecessary curr
      maybeChangeValue curr cv
    Invalid                -> error "Invalid node should not be in the recompute heap"
    Map f cref             -> (valueExn cref) >>= \cv -> maybeChangeValue curr (f cv)
    Uninitialized          -> error "Current node is uninitialized"
    Variable (Var v _ _ _) -> maybeChangeValue curr v

    where maybeChangeValue :: Eq a => NodeRef a -> a -> StateIO ()
          maybeChangeValue ref new_v = do
            -- TODO: Cutoff.should_cutoff
            old_v <- getNodeValue ref
            when (isNothing old_v || not (new_v == fromJust old_v)) $ do
              setNodeValue ref (Just new_v)
              updateChangedAt ref
              modify (\s -> s & info.debug.nodesChanged %~ (+ 1))
              n <- readIORefT (getRef ref)
              when (n^.handlers.numOnUpdates > 0)
                   (modifyIORefT (getRef ref) (\x -> x & node.value.oldValue .~ old_v))
              -- TODO: .....
              sequence_ $ N.iteriParents n (\_ pn -> addToRecomputeHeap pn)

recomputeP :: PackedNode -> StateIO ()
recomputeP (PackedNode noderef) = recompute noderef

recomputeEverythingThatIsNecessary :: StateIO ()
recomputeEverythingThatIsNecessary = do
  env <- get
  let old_heap = env^.recHeap
  if (RH.isEmpty old_heap)
     then return ()
     else do let (min_node, new_heap) = RH.pop1 old_heap
             modify (\s -> s & recHeap .~ new_heap)
             recomputeP min_node
             recomputeEverythingThatIsNecessary

--------------------------------- Node Related Helper --------------------------
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

addToRecomputeHeap :: PackedNode -> StateIO ()
addToRecomputeHeap pn = do
  init_h <- getHeightP pn
  modify (\s -> s & recHeap %~ (RH.add (init_h, pn)))
---------------------------------- Helper --------------------------------------
valueExn :: Eq a => NodeRef a -> StateIO a
valueExn (Ref ref _) = readIORefT ref >>= (return . N.valueExn)

readIORefT :: (IORef a) -> StateIO a
readIORefT = (lift . readIORef)

modifyIORefT :: (IORef a) -> (a -> a) -> StateIO ()
modifyIORefT ref g = lift $ modifyIORef ref g

-- | DFS parent nodes with a function 'check'. If 'check' returns True, then
-- continue searching for parent; otherwise, return
-- 'check' :: Maybe child -> current -> t IO Bool
dfsParentWithoutRepeat :: (MonadTrans t, Monad (t IO), Eq a)
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

dfsParentWithRepeat :: (MonadTrans t, Monad (t IO), Eq a)
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
  n1 <- newNode :: StateIO (NodeRef Int)
  n2 <- newNode :: StateIO (NodeRef Int)
  n3 <- newNode :: StateIO (NodeRef Int)
  n4 <- newNode :: StateIO (NodeRef Int)
  addParent n1 n2
  addParent n1 n3
  addParent n2 n4
  addParent n3 n4
  return [pack n1, pack n2, pack n3, pack n4]

testPrintNodeInfo :: Maybe PackedNode -> PackedNode -> StateIO Bool
testPrintNodeInfo _ pn = do
  h <- getHeightP pn
  (lift . putStrLn) $ "Reach node id = " ++ show pn ++ " with height = " ++ show h
  return True

-- | try to do dfs from a node and print the node id visited
testDfsParent :: StateIO ()
testDfsParent = do
  nodes <- testCreateGraph
  dfsParentWithoutRepeatP (nodes !! 0) testPrintNodeInfo

runTest :: StateIO a -> IO ()
runTest action = runStateT action initState >> return ()
