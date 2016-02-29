module State where
import Lens.Simple
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad (when, foldM)
import Data.IORef
import Data.Set (Set)
import Data.Maybe(isNothing, fromJust)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Unique
import Prelude hiding (all)

import Types
import Utils
import qualified Node as N
import qualified Recompute_Heap as RH
import qualified Observer as O
import qualified Var      as V

createNode :: Eq a => StateIO (NodeRef a)
createNode = lift N.createNode

getStbNum:: StateIO Int
getStbNum = do s <- get; return $ s^.info.stbNum

amStabilizing :: StateIO Bool
amStabilizing = do
  s <- get
  case (s^.info.status) of
    NotStabilizing            -> return False
    StabilizePreviouslyRaised -> return False
    Stabilizing               -> return True

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
  modify (\s -> s & info.debug.nodesBecame.unnecessary %~ (+ 1))
  modifyIORefT ref (\n0 -> n0 & height .~ (-1))
  removeChildren nf
  -- TODO: kind = Unordered_array_fold / UnorderedArrayFold
  removeFromRecHeap (pack nf)

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
       N.setNodeValue ref Nothing
       N.updateChangedAt ref
       N.updateRecomputedAt ref
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
  ch <- N.getHeightP cpn
  ph <- N.getHeightP ppn
  if (ch < ph)
     then return False
     else do
       N.setHeight pref (ch + 1)
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
becameNecessary (Ref _ _) = do
  -- TODO: add fail conditions
  modify (\s -> s & info.debug.nodesBecame.necessary %~ (+ 1))
  -- TODO: when stale, add to recompute heap
  -- TODO: https://github.com/janestreet/incremental/blob/master/src/state.ml#L476

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
  N.updateRecomputedAt curr
  n0 <- readIORefT (getRef curr)
  case n0^.node.kind of
    Const x                -> maybeChangeValue curr x
    Freeze _ cref f        -> do
      cv <- valueExn cref
      when (f cv) $ do removeChildren curr
                       lift $ N.setKind curr (Const cv)
                       if (N.isNecessary n0)
                          then N.setHeight curr 0
                          else becameUnnecessary curr
      maybeChangeValue curr cv
    Invalid                -> error "Invalid node should not be in the recompute heap"
    Map f cref             -> (valueExn cref) >>= \cv -> maybeChangeValue curr (f cv)
    Uninitialized          -> error "Current node is uninitialized"
    Variable v0 _ _        -> maybeChangeValue curr v0

    where maybeChangeValue :: Eq a => NodeRef a -> a -> StateIO ()
          maybeChangeValue ref new_v = do
            -- TODO: Cutoff.should_cutoff
            old_v <- N.getNodeValue ref
            when (isNothing old_v || not (new_v == fromJust old_v)) $ do
              N.setNodeValue ref (Just new_v)
              N.updateChangedAt ref
              modify (\s -> s & info.debug.nodesChanged %~ (+ 1))
              n <- readIORefT (getRef ref)
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

addToRecomputeHeap :: PackedNode -> StateIO ()
addToRecomputeHeap pn = do
  init_h <- N.getHeightP pn
  modify (\s -> s & recHeap %~ (RH.add (init_h, pn)))

---------------------------------- Observers ----------------------------------
unlinkDisallowedObs :: StateIO ()
unlinkDisallowedObs = do
  env <- get
  mapM_ unlink0 $ env^.observer.disallowed
  modify (\s -> s & observer.disallowed .~ [])
    where unlink0 i = do
            (PackObs o) <- O.getObsByID i
            -- TODO: change to 'assert'
            when (_state o /= Disallowed) $
                 error "State.unlinkDisallowedObs assertion failed"
            O.modifyObsState i Unlinked
            O.unlink o
            checkIfUnnecessary (o^.observing)

disallowFutureUse :: Observer a -> StateIO ()
disallowFutureUse (Obs i s0 _)
  | s0 == Created = do
       modify (\s -> s & observer.numActive %~ ((-) 1))
       O.modifyObsState i Unlinked
  | s0 == InUse  = do
       modify (\s -> s & observer.numActive %~ ((-) 1))
       modify (\s -> s & observer.disallowed %~ (i :))
       O.modifyObsState i Disallowed
  | otherwise = return ()

createObserver :: Eq a => NodeRef a -> StateIO (Observer a)
createObserver nf = do
  i   <- lift newUnique
  let obs = Obs i Created nf
  modify (\s -> s & observer.numActive %~ (+ 1))
  modify (\s -> s & observer.new %~ (PackObs obs :))
  return obs

addNewObservers :: StateIO ()
addNewObservers = do
  env <- get
  mapM_ add0 $ env^.observer.new
  modify (\s -> s & observer.new .~ [])
    where
    add0 (PackObs o) = case (_state o) of
      InUse      -> error "State.addNewObservers observer is in use"
      Disallowed -> error "State.addNewObservers observer is disallowed"
      Unlinked   -> return ()
      Created    -> do
        let newobs         = PackObs (o{_state = InUse})
            obs_id         = o^. obsID
            nf@(Ref ref _) = o^. observing
        was_necessary <- (readIORefT ref) >>= return . N.isNecessary
        modify (\s -> s & observer.all %~ (Map.insert obs_id newobs))
        modifyIORefT ref (\n  -> n & obsOnNode %~ (Set.insert obs_id))
        when (not was_necessary) $ becameNecessary nf

---------------------------------- Var ----------------------------------
createVar :: Eq a => a -> StateIO (Var a)
createVar v0 = do
  --TODO scope
  watched <- createNode
  stbnum  <- getStbNum
  let var = Variable v0 Nothing stbnum
  lift $ N.setKind watched var
  return (Var watched)

setVarWhileNotStabilizing :: Eq a => Var a -> a -> StateIO ()
setVarWhileNotStabilizing (Var (Ref ref _)) new_v = do
  modify (\s -> s & info.debug.varSets %~ (+ 1))
  watched <- readIORefT ref
  stbnum  <- getStbNum
  let old_k = watched^.node.kind
      new_k = old_k{mvalue = new_v}
  if (setAt old_k < stbnum)
     then do modifyIORefT ref (\n -> n & node.kind .~ new_k{setAt = stbnum})
             -- when (N.isNecessary watched && not_in_rec_heap) (add to heap)
     else modifyIORefT ref (\n -> n & node.kind .~ new_k)

setVar :: Eq a => Var a -> a -> StateIO ()
setVar v0@(Var (Ref ref _)) new_v = do
  s0 <- get
  case (s0^.info.status) of
    NotStabilizing            -> setVarWhileNotStabilizing v0 new_v
    StabilizePreviouslyRaised ->
      error "Cannot set var -- stabilization previously raised"
    Stabilizing               -> do
      watched <- readIORefT ref
      let old_k = watched^.node.kind
      when (isNothing $ valueSetDuringStb old_k)
           (modify (\s -> s & varSetDuringStb %~ (PackVar v0 :)))
      modifyIORefT ref (\n -> n & node.kind .~ old_k{valueSetDuringStb = Just new_v})

---------------------------------- Stabilization ------------------------------
stabilize :: StateIO ()
stabilize = do
  s0 <- get
  modify (\s -> s & info.status .~ Stabilizing)
  -- disallow finailized observers
  addNewObservers
  -- unlink disallowed observers
  recomputeEverythingThatIsNecessary
  modify (\s -> s & info.stbNum %~ (+ 1))
  mapM_ go (s0^.varSetDuringStb)
  modify (\s -> s & varSetDuringStb .~ [])
  -- handler
  modify (\s -> s& info.status .~ NotStabilizing)
  -- TODO: add try-catch execption control flow
    where go (PackVar var) = do
            watched <- readIORefT (getRef $ watch var)
            let old_k = watched^.node.kind
                v0    = fromJust $ valueSetDuringStb old_k
            modifyIORefT (getRef $ watch var)
                         (\n -> n & node.kind .~ old_k{valueSetDuringStb = Nothing})
            setVarWhileNotStabilizing var v0

---------------------------------- Helper -------------------------------------
valueExn :: Eq a => NodeRef a -> StateIO a
valueExn (Ref ref _) = readIORefT ref >>= (return . N.valueExn)


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
  n1 <- createNode :: StateIO (NodeRef Int)
  n2 <- createNode :: StateIO (NodeRef Int)
  n3 <- createNode :: StateIO (NodeRef Int)
  n4 <- createNode :: StateIO (NodeRef Int)
  addParent n1 n2
  addParent n1 n3
  addParent n2 n4
  addParent n3 n4
  return [pack n1, pack n2, pack n3, pack n4]

testPrintNodeInfo :: Maybe PackedNode -> PackedNode -> StateIO Bool
testPrintNodeInfo _ pn = do
  h <- N.getHeightP pn
  (lift . putStrLn) $ "Reach node id = " ++ show pn ++ " with height = " ++ show h
  return True

-- | try to do dfs from a node and print the node id visited
testDfsParent :: StateIO ()
testDfsParent = do
  nodes <- testCreateGraph
  dfsParentWithoutRepeatP (nodes !! 0) testPrintNodeInfo

testVar :: StateIO ()
testVar = do
  v1    <- createVar 5
  curr1 <- V.getValue v1
  (lift . putStrLn) $ "value for v1 is " ++ show curr1
  setVar v1 7
  curr10 <- V.getValue v1
  curr11 <- V.getLatestValue v1
  (lift . putStrLn) $ "value for v1 is " ++ show curr10
  (lift . putStrLn) $ "latest value for v1 is " ++ show curr11
  stabilize
  curr12 <- V.getValue v1
  (lift . putStrLn) $ "value for v1 is " ++ show curr12
    -- hit exception because initial recompute heap is not defined

runTest :: StateIO a -> IO ()
runTest action = runStateT action initState >> return ()

