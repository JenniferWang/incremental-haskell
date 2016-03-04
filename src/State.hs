module State where
import Lens.Simple
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad (when, foldM)
import Data.Traversable (mapM)
import Data.IORef
import Data.Set (Set)
import Data.Maybe(isNothing, fromJust)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Unique
import Prelude hiding (all)

import Types
import Utils
import qualified Node     as N
import qualified Observer as O
import qualified Var      as V

---------------------------------- Node ----------------------------------
createNode :: Eq a => Kind a -> StateIO (NodeRef a)
createNode = lift . N.createNode

getStbNum:: StateIO Int
getStbNum = do s <- get; return $ s^.info.stbNum

amStabilizing :: StateIO Bool
amStabilizing = do
  s <- get
  case (s^.info.status) of
    NotStabilizing            -> return False
    StabilizePreviouslyRaised -> return False
    Stabilizing               -> return True

-- | This function actually only removes the parent node from its children
-- it does not remove the actual children from parent node, which is
-- stored in 'kind' because later, the parent's kind would be set to
-- 'Invalid' or remain 'Freeze'
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

-- | Make a node unnecessary, remove it's children
becameUnnecessary :: Eq a => NodeRef a -> StateIO ()
becameUnnecessary nf = do
  modify (\s -> s & info.debug.nodesBecame.unnecessary %~ (+ 1))
  removeChildren nf
  -- if the node is not in recompute heap, this will do nothing
  removeFromRecHeap (pack nf)

-- | invalidate a valid node
-- This is a function used in 'propagateInvalidity' with dfs search for parents
-- https://github.com/janestreet/incremental/blob/master/src/state.ml#L357
invalidateNode :: Maybe PackedNode -> PackedNode -> StateIO Bool
invalidateNode _ pn@(PackedNode ref) = do
  n    <- readIORefT (getRef ref)
  -- TODO: confusing ... flag = true if n has invalid children
  flag <- (lift . N.shouldBeInvalidated) n
  if not (N.isValid n || flag)
     then return False
     else do
       when verbose (putStrLnT "--* adding invalid node, invalidating parents")
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

addParent :: (Eq a, Eq b) => NodeRef a -> NodeRef b -> StateIO ()
addParent child_ref par_ref = do
  putStrLnT $ "--* State.addParent from child " ++ show child_ref ++ " to " ++ show par_ref
  child  <- readIORefT (getRef child_ref)
  let was_necessary = N.isNecessary child
  -- when (not $ N.isNecessary parent) (error "We only add necessary parent")
  lift $ N.addParent child_ref par_ref
    -- if we add an invalid node to a parent => invalidate all the parents
  when (not $ N.isValid child) $ propagateInvalidity child_ref
  when (not was_necessary)     $ becameNecessary child_ref

-- | This is the main function
becameNecessary :: Eq a => NodeRef a -> StateIO ()
becameNecessary parent@(Ref pref _) = do
  -- TODO: add fail conditions
  putStrLnT $ "--* State.becameNecessary " ++ show parent
  modify (\s -> s & info.debug.nodesBecame.necessary %~ (+ 1))
  par_node <- readIORefT pref
  sequence_ $
    N.iteriChildren par_node (\_ (PackedNode child) -> addParent child parent)
  is_stale <- N.isStaleP parent
  when is_stale $ addToRecomputeHeap (pack parent)

removeFromRecHeap :: PackedNode -> StateIO ()
removeFromRecHeap pn = modify (\s -> s & recHeap %~ (Set.delete pn))

changeChild :: (Eq a, Eq b) => NodeRef a -> NodeRef b -> NodeRef b -> StateIO ()
changeChild parent old_child new_child = do
  if old_child == new_child
     then return ()
     else do
       lift $ N.removeParent old_child parent
       -- TODO: force necessary?
       addParent new_child parent
       checkIfUnnecessary old_child

-- Recompute the value of current node
recompute :: PackedNode -> StateIO ()
recompute (PackedNode nf) = do
  when verbose (putStrLnT $ "--* State.recompute node " ++ show nf)
  modify (\s -> s & info.debug.nodesRecomputed.byDefault %~ (+ 1))
  N.updateRecomputedAt nf
  n0 <- readIORefT (getRef nf)
  case n0^.kind of
    Const x                -> maybeChangeValue nf x
    Freeze _ cref f        -> do
      cv <- valueExn cref
      when (f cv) $ do removeChildren nf
                       lift $ N.setKind nf (Const cv)
                       if (N.isNecessary n0)
                          -- original : then N.setHeight curr 0
                          -- because as it could not have any children, the height = 0
                          then return ()
                          else becameUnnecessary nf
      maybeChangeValue nf cv
    Invalid                -> error "Invalid node should not be in the recompute heap"
    Uninitialized          -> error "Current node is uninitialized"
    Variable v0 _ _        -> maybeChangeValue nf v0
    Map f b                -> (valueExn b) >>= \b' -> maybeChangeValue nf (f b')
    Map2 f b c             -> do
      x <- valueExn b
      y <- valueExn c
      maybeChangeValue nf (f x y)
    where maybeChangeValue :: Eq a => NodeRef a -> a -> StateIO ()
          maybeChangeValue ref new_v = do
            -- TODO: Cutoff.should_cutoff
            old_v <- N.getNodeValue ref
            when (isNothing old_v || not (new_v == fromJust old_v)) $ do
              N.setNodeValue ref (Just new_v)
              N.updateChangedAt ref
              modify (\s -> s & info.debug.nodesChanged %~ (+ 1))
              -- TODO: .....

recomputeEverythingThatIsNecessary :: StateIO ()
recomputeEverythingThatIsNecessary = do
  env <- get
  let roots = Set.toList $ env^.recHeap
  when (verbose) (putStrLnT $ "--* Current root set is " ++ show roots)
  -- check if the graph is DAG
  is_cyclic <- foldM go False roots
  when (is_cyclic) $ error "Cycle detected! The graph is not DAG"
  -- topological sort: dfs + list
  (stack, _) <- topo roots [] Set.empty
  mlapM_ recompute stack
    where
      topo []     stack seen = return (stack, seen)
      topo (x:xs) stack seen
        | x `Set.member` seen = topo xs stack seen
        | otherwise = do
            xs' <- N.getParentsP x
            (stack', seen') <- topo xs' stack (Set.insert x seen)
            topo xs (x:stack') seen'

      cyclic path x
        | x `Set.member` path = return True
        | otherwise = do
            parents0 <- N.getParentsP x
            if parents0 == []
               then return False
               else or <$> (mapM (cyclic (Set.insert x path)) parents0)

      go True  _    = return True
      go False root = cyclic Set.empty root

-- TODO: marked as inline
addToRecomputeHeap :: PackedNode -> StateIO ()
addToRecomputeHeap pn = do
  modify (\s -> s & recHeap %~ (Set.insert pn))

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

disallowFutureUse :: InterObserver a -> StateIO ()
disallowFutureUse (InterObs i s0 _)
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
  let obs = InterObs i Created nf
  modify (\s -> s & observer.numActive %~ (+ 1))
  modify (\s -> s & observer.new %~ (PackObs obs :))
  return (Obs i)

addNewObservers :: StateIO ()
addNewObservers = do
  putStrLnT "--* adding new observers to global state"
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
        modifyIORefT ref (\n  -> n & edges.obsOnNode %~ (Set.insert obs_id))
        when (not was_necessary) $ becameNecessary nf

---------------------------------- Var ----------------------------------
-- Create a new node with kind = Variable. Return a proxy to the node
-- This will not add the node to the DAG
createVar :: Eq a => a -> StateIO (Var a)
createVar v0 = do
  --TODO scope
  stbnum <- getStbNum
  let var = Variable v0 Nothing stbnum
  watched <- createNode var
  when verbose (putStrLnT $ "--* New Var created " ++ show watched)
  return (Var watched)

setVarWhileNotStabilizing :: Eq a => Var a -> a -> StateIO ()
setVarWhileNotStabilizing (Var nf@(Ref ref _)) new_v = do
  modify (\s -> s & info.debug.varSets %~ (+ 1))
  watched <- readIORefT ref
  stbnum  <- getStbNum
  let old_k = watched^.kind
      new_k = old_k{mvalue = new_v}
  if (setAt old_k < stbnum)
     then do modifyIORefT ref (\n -> n & kind .~ new_k{setAt = stbnum})
             when (N.isNecessary watched) (addToRecomputeHeap $ pack nf)
             -- when (N.isNecessary watched && not_in_rec_heap) (add to heap)
     else modifyIORefT ref (\n -> n & kind .~ new_k)

setVar :: Eq a => Var a -> a -> StateIO ()
setVar v0@(Var (Ref ref _)) new_v = do
  s0 <- get
  case (s0^.info.status) of
    NotStabilizing            -> setVarWhileNotStabilizing v0 new_v
    StabilizePreviouslyRaised ->
      error "Cannot set var -- stabilization previously raised"
    Stabilizing               -> do
      watched <- readIORefT ref
      let old_k = watched^.kind
      when (isNothing $ valueSetDuringStb old_k)
           (modify (\s -> s & varSetDuringStb %~ (PackVar v0 :)))
      modifyIORefT ref (\n -> n & kind .~ old_k{valueSetDuringStb = Just new_v})

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
            let old_k = watched^.kind
                v0    = fromJust $ valueSetDuringStb old_k
            modifyIORefT (getRef $ watch var)
                         (\n -> n & kind .~ old_k{valueSetDuringStb = Nothing})
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

---------------------------------- Toy Test --------------------------------------
verbose :: Bool
verbose = True

printObs :: (Show a, Eq a) => Observer a -> StateIO ()
printObs obs = O.obsValueExn obs
           >>= \x -> putStrLnT $ show obs ++ " = " ++ show x

printVar :: (Show a, Eq a) => Var a -> StateIO ()
printVar x = V.getValue x
          >>= \y -> putStrLnT $ show x ++ " = " ++ show y
-- | Create a graph for testing
-- testCreateGraph :: StateIO [PackedNode]
-- testCreateGraph = do
--   n1 <- createNode :: StateIO (NodeRef Int)
--   n2 <- createNode :: StateIO (NodeRef Int)
--   n3 <- createNode :: StateIO (NodeRef Int)
--   n4 <- createNode :: StateIO (NodeRef Int)
--   addParent n1 n2
--   addParent n1 n3
--   addParent n2 n4
--   addParent n3 n4
--   return [pack n1, pack n2, pack n3, pack n4]

-- testPrintNodeInfo :: Maybe PackedNode -> PackedNode -> StateIO Bool
-- testPrintNodeInfo _ pn = do
--   h <- N.getHeightP pn
--   (lift . putStrLn) $ "Reach node id = " ++ show pn ++ " with height = " ++ show h
--   return True

-- | try to do dfs from a node and print the node id visited
-- testDfsParent :: StateIO ()
-- testDfsParent = do
--   nodes <- testCreateGraph
--   dfsParentWithoutRepeatP (nodes !! 0) testPrintNodeInfo

printParents :: Var a -> StateIO ()
printParents var = do
  n <- readIORefT $ getRef (watch var)
  putStrLnT $ show (N.getParents n)

testVar :: StateIO ()
testVar = do
  v1    <- createVar 5
  v2    <- createNode (Map (+ 6) (watch v1))
  -- obs   <- createObserver v2
  putStrLnT "All nodes are added"

  stabilize
  printVar v1
  -- printObs obs

  setVar v1 10
  stabilize
  printVar v1
  -- printObs obs

runTest :: StateIO a -> IO ()
runTest action = runStateT action initState >> return ()

