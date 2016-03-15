module State where
import Lens.Simple
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad (when, foldM, foldM_)
import Control.Monad.Catch (finally)
import Control.Concurrent.Async (async, wait)
import Data.IORef
import Data.Maybe(isNothing, fromJust)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Unique
import Prelude hiding (all)

import Types
import Utils
import Var (getValue)
import qualified Node      as N
import qualified Observer  as O
import qualified ArrayFold as AF

---------------------------------- Node ----------------------------------
createNodeIn :: Eq a => Scope -> Kind a -> StateIO (NodeRef a)
createNodeIn created_in  k = do
  modify (\s -> s & info.debug.nodesCreated %~ (+ 1))
  N.create created_in k

createNode :: Eq a => Kind a -> StateIO (NodeRef a)
createNode k = getCurrScope >>= \s -> createNodeIn s k

createNodeTop :: Eq a => Kind a -> StateIO (NodeRef a)
createNodeTop = createNodeIn Top

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
checkIfUnnecessary ref = do
  n <- readNodeRef ref
  if (N.isNecessary n) then return ()
                       else becameUnnecessary ref

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
  n    <- readNodeRef ref
  -- TODO: confusing ... flag = true if n has invalid children
  flag <- N.shouldBeInvalidated n
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
       N.setKind ref Invalid
       -- remove invalid nodes from the recompute heap
       removeFromRecHeap pn
       return True

-- | use dfs to propagate Invalidity to parent nodes
propagateInvalidity :: Eq a => NodeRef a -> StateIO ()
propagateInvalidity ref = dfsParentWithoutRepeat ref invalidateNode

addParent :: (Eq a, Eq b) => NodeRef a -> NodeRef b -> StateIO ()
addParent child_ref par_ref = do
  child  <- readNodeRef child_ref
  if (N.isParent (pack par_ref) child)
     then return ()
     else do
       when verbose (putStrLnT $ "--* State.addParent from child " ++ show child_ref ++ " to " ++ show par_ref)
       let was_necessary = N.isNecessary child
       -- when (not $ N.isNecessary parent) (error "We only add necessary parent")
       N.addParent child_ref par_ref
         -- if we add an invalid node to a parent => invalidate all the parents
       when (not $ N.isValid child) $ propagateInvalidity child_ref
       when (not was_necessary)     $ becameNecessary child_ref

becameNecessary :: Eq a => NodeRef a -> StateIO ()
becameNecessary parent = do
  -- TODO: add fail conditions
  when verbose (putStrLnT $ "--* State.becameNecessary " ++ show parent)
  modify (\s -> s & info.debug.nodesBecame.necessary %~ (+ 1))
  par_node <- readNodeRef parent
  sequence_ $
    N.iteriChildren par_node (\_ (PackedNode child) -> addParent child parent)
  is_stale <- N.isStaleP parent
  when is_stale $ addToRecomputeHeap (pack parent)

removeFromRecHeap :: PackedNode -> StateIO ()
removeFromRecHeap pn = modify (\s -> s & recHeap %~ (Set.delete pn))

-- Recompute the value of current node without checking if the current node is stale or not
recompute :: PackedNode -> StateIO ()
recompute (PackedNode nf) = do
  n0     <- readNodeRef nf
  stbnum <- getStbNum
  if ((n0^.value.recomputedAt) >= stbnum)
     then return ()
     else do
        when verbose (putStrLnT $ "--* State.recompute node " ++ show nf)
        modify (\s -> s & info.debug.nodesRecomputed.byDefault %~ (+ 1))
        N.updateRecomputedAt nf
        case n0^.kind of
          ArrayFold init f array -> AF.compute f init array >>= maybeChangeValue nf
          Const x                -> maybeChangeValue nf x
          Freeze _ cref f        -> do
            cv <- valueExn cref
            when (f cv) $ do removeChildren nf
                             N.setKind nf (Const cv)
                             if (N.isNecessary n0)
                                -- original : then N.setHeight curr 0
                                -- because as it could not have any children, the height = 0
                                then return ()
                                else becameUnnecessary nf
            maybeChangeValue nf cv

          Invalid                -> error "Invalid node should not be in the recompute heap"
          Uninitialized          -> error "Current node is uninitialized"
          Variable v0 _ _        -> maybeChangeValue nf v0
          Map  f b               -> app1 f b >>= maybeChangeValue nf
          Map2 f b c             -> app2 f b c >>= maybeChangeValue nf
          Map3 f b c d           -> app3 f b c d >>= maybeChangeValue nf
          Map4 f b c d e         -> app4 f b c d e >>= maybeChangeValue nf
          -- Bind node is tricky.
          --   a. [rhs] is Nothing, it should first compute [rhs] and
          --     then add the edge [rhs] -> bind node [nf]
          --   b. [rhs] is Just, there could be three cases which incurs the
          --     recompute of the bind node.
          --     1. Some node created OUTSIDE changes => don't really need to
          --        invalidate nodes CREATED in rhs, a.k.a. nodes_r
          --     2. The node on lhs changes => need to invalidate all the nodes
          --        CREATED in rhs.
          --     3. Both of the two cases occur

          Bind f l r nodes_r -> do
            putStrLnT $ "State.recompute Bind node, nodes created on rhs " ++ show nodes_r
            modifyNodeRef nf (\n -> n & kind %~ (\k -> k{ nodesCreatedInScope = [] }))
            lhs_node <- readNodeRef l
            new_rhs  <- runWithScope (Bound nf) (\() -> f (N.valueExn lhs_node))
            modifyNodeRef nf (\n -> n & kind %~ (\k -> k{ rhs = Just new_rhs }))

            update nf r new_rhs
            addParent new_rhs nf
            copyChild nf new_rhs
              where
              update :: Eq a => NodeRef a -> Maybe (NodeRef a) -> NodeRef a -> StateIO ()
              update _ Nothing new_child = recomputeFromParent (pack new_child)
              update parent (Just old_child) new_child = do
                if old_child == new_child
                   then return ()
                   else do
                     when verbose (putStrLnT $ "--* State.remove parent from child " ++ show old_child
                                  ++ " to " ++ show parent)
                     lift $ N.removeParent old_child parent
                     invalidateNodesCreatedOnRHS nodes_r
                     recomputeFromParent (pack new_child)
                     checkIfUnnecessary old_child

maybeChangeValue :: Eq a => NodeRef a -> a -> StateIO ()
maybeChangeValue ref new_v = do
  -- TODO: Cutoff.should_cutoff
  node  <- readNodeRef ref
  let old_v = node^.value.v
  when (isNothing old_v || not (new_v == fromJust old_v)) $ do
    N.setNodeValue ref (Just new_v)
    N.updateChangedAt ref
    modify (\s -> s & info.debug.nodesChanged %~ (+ 1))

-- Recompute from children sets. (From child nodes to parent nodes)
-- a node cannot be recomputed more than once in one stabilization
recomputeFromChildren :: [PackedNode] -> StateIO ()
recomputeFromChildren roots = do
  (stack, _) <- topo roots [] Set.empty
  mapM_ recompute stack
  where
    topo []     stack seen = return (stack, seen)
    topo (x:xs) stack seen
      | x `Set.member` seen = topo xs stack seen
      | otherwise = do
          -- Only fetch parents in the top scope or the bind node
          xs'             <- N.getTopParentsP x
          (stack', seen') <- topo xs' stack (Set.insert x seen)
          topo xs (x:stack') seen'

-- Recompute from a parent. This is only used to compute rhs nodes.
-- TODO:It might be expensive?
recomputeFromParent :: PackedNode -> StateIO ()
recomputeFromParent p@(PackedNode par) = do
  parent <- readNodeRef par
  -- update children
  let was_necessary = N.isNecessary parent
  sequence_ $ N.iteriChildren parent (\_ c -> recomputeFromParent c)
  recompute p
  when (not was_necessary) (becameNecessary par)

recomputeEverythingThatIsNecessary :: StateIO ()
recomputeEverythingThatIsNecessary = do
  env <- get
  let roots = Set.toList $ env^.recHeap
  when (verbose) (putStrLnT $ "--* Current root set is " ++ show roots)
  -- check if the graph is DAG
  is_cyclic <- foldM go False roots
  when (is_cyclic) $ error "Cycle detected! The graph is not DAG"
  -- topological sort: dfs + list
  recomputeFromChildren roots
  -- clear the recompute heap
  modify (\s -> s & recHeap .~ Set.empty)
  where
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

invalidateNodesCreatedOnRHS :: [PackedNode] -> StateIO ()
invalidateNodesCreatedOnRHS = foldM_ (\() (PackedNode nf) -> propagateInvalidity nf) ()

-- Run a user-give fuction in specific scope.
runWithScope :: Eq a => Scope -> (() -> StateIO (NodeRef a)) -> StateIO (NodeRef a)
runWithScope scope f = do
  saved <- getCurrScope
  modify (\s -> s & info.currScope .~ scope)
  f () `finally` modify (\s -> s & info.currScope .~ saved)

copyChild :: Eq a => NodeRef a -> NodeRef a -> StateIO ()
copyChild pref cref = do
  child <- readNodeRef cref
  if (N.isValid child)
     then maybeChangeValue pref (N.valueExn child)
     else propagateInvalidity pref

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
  env <- get
  mapM_ add0 $ env^.observer.new
  modify (\s -> s & observer.new .~ [])
    where
    add0 (PackObs o) = case (_state o) of
      InUse      -> error "State.addNewObservers observer is in use"
      Disallowed -> error "State.addNewObservers observer is disallowed"
      Unlinked   -> return ()
      Created    -> do
        let newobs = PackObs (o{_state = InUse})
            obs_id = o^. obsID
            ref    = o^. observing
        was_necessary <- readNodeRef ref >>= return . N.isNecessary
        modify (\s -> s & observer.all %~ (Map.insert obs_id newobs))
        modifyNodeRef ref (\n  -> n & edges.obsOnNode %~ (Set.insert obs_id))
        when (not was_necessary) $ becameNecessary ref

--------------------------------- Const ---------------------------------------
const :: Eq a => a -> StateIO (NodeRef a)
const v = createNode (Const v)

---------------------------------- Var ----------------------------------------
-- Create a new node with kind = Variable. Return a proxy to the node
-- This will not add the node to the DAG
createVar :: Eq a => Bool -> a -> StateIO (Var a)
createVar use_current_scope v = do
  stbnum <- getStbNum
  curr   <- getCurrScope
  let var = Variable v Nothing stbnum
      scope = if use_current_scope then curr else Top
  watched <- createNodeIn scope var
  when verbose (putStrLnT $ "--* New Var created " ++ show watched)
  return (Var watched)

setVarWhileNotStabilizing :: Eq a => Var a -> a -> StateIO ()
setVarWhileNotStabilizing (Var ref) new_v = do
  modify (\s -> s & info.debug.varSets %~ (+ 1))
  watched <- readNodeRef ref
  stbnum  <- getStbNum
  let old_k = watched^.kind
      new_k = old_k{ mvalue = new_v }
  if (setAt old_k < stbnum)
     then do modifyNodeRef ref (\n -> n & kind .~ new_k{ setAt = stbnum })
             when (N.isNecessary watched) (addToRecomputeHeap $ pack ref)
             -- when (N.isNecessary watched && not_in_rec_heap) (add to heap)
     else modifyNodeRef ref (\n -> n & kind .~ new_k)

setVar :: Eq a => Var a -> a -> StateIO ()
setVar v0@(Var ref) new_v = do
  s0 <- get
  case (s0^.info.status) of
    NotStabilizing            -> setVarWhileNotStabilizing v0 new_v
    StabilizePreviouslyRaised ->
      error "Cannot set var -- stabilization previously raised"
    Stabilizing               -> do
      watched <- readNodeRef ref
      let old_k = watched^.kind
      when (isNothing $ valueSetDuringStb old_k)
           (modify (\s -> s & varSetDuringStb %~ (PackVar v0 :)))
      modifyNodeRef ref (\n -> n & kind .~ old_k{valueSetDuringStb = Just new_v})

---------------------------------- Bind ---------------------------------------
-- 'bind' gives the flexibility to dynamically change the DAG
-- invariant: node created outside the bind scope should not be modified within the scope

bind :: (Eq a, Eq b) => (NodeRef a) -> (a -> StateIO (NodeRef b)) -> StateIO (NodeRef b)
bind lhs_node f = do
  bind_node <- createNodeTop (Bind f lhs_node Nothing [])
  return bind_node


---------------------------------- Map ----------------------------------------
map n f = createNode (Map f n)

map2 n1 n2 f = createNode (Map2 f n1 n2)

map3 n1 n2 n3 f = createNode (Map3 f n1 n2 n3)

map4 n1 n2 n3 n4 f = createNode (Map4 f n1 n2 n3 n4)

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

---------------------------------- Freeze -------------------------------------
freeze :: Eq a => NodeRef a -> (a -> Bool) -> StateIO (NodeRef a)
freeze child only_freeze_when = do
  new_node <- createNodeTop Uninitialized
  N.setKind new_node (Freeze new_node child only_freeze_when)
  addParent child new_node
  return new_node

---------------------------------- Helper -------------------------------------
valueExn :: Eq a => NodeRef a -> StateIO a
valueExn ref = readNodeRef ref >>= (return . N.valueExn)

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

---------------------------------- Helper ---------------------------------------
app1 f b = valueExn b >>= return . f

app2 f b c = valueExn b >>= \bv -> app1 (f bv) c

app3 f b c d = valueExn b >>= \bv -> app2 (f bv) c d

app4 f b c d e = valueExn b >>= \bv -> app3 (f bv) c d e

---------------------------------- Toy Test --------------------------------------
printObs :: (Show a, Eq a) => Observer a -> StateIO ()
printObs obs = O.obsValueExn obs
           >>= \x -> putStrLnT $ show obs ++ " = " ++ show x

printVar :: (Show a, Eq a) => Var a -> StateIO ()
printVar x = Var.getValue x
          >>= \y -> putStrLnT $ show x ++ " = " ++ show y -- | Create a graph for testing

printParents :: Var a -> StateIO ()
printParents var = do
  n <- readIORefT $ getRef (watch var)
  putStrLnT $ show (N.getParents n)

testMap :: StateIO ()
testMap = do
  v1    <- createVar False (5 :: Int)
  v2    <- createNodeTop (Map (+ 6) (watch v1))
  -- obs   <- createObserver v2
  putStrLnT "All nodes are added"

  stabilize
  printVar v1
  -- printObs obs

  setVar v1 10
  stabilize
  printVar v1
  -- printObs obs

testMap2 :: StateIO ()
testMap2 = do
  v1 <- createVar False (5 :: Int)
  v2 <- createVar False 10
  n  <- createNodeTop (Map2 (+) (watch v1) (watch v2))
  ob <- createObserver n
  putStrLnT "All nodes are added"

  stabilize
  printObs ob
  putStrLnT "After first stabilization"

  setVar v1 7
  -- setVar v2 11
  stabilize
  printObs ob
  putStrLnT "After second stabilization"

if_ :: Eq a
    => NodeRef Bool -> NodeRef a -> NodeRef a -> StateIO (NodeRef a)
if_ a b c = bind a (\x -> if x then return b else return c)

testBind1 :: StateIO ()
testBind1 = do
  flag   <- createVar False True
  then_  <- createVar True (5 :: Int)
  else_  <- createVar True 6
  try_if <- if_ (watch flag) (watch then_) (watch else_)
  ob     <- createObserver try_if

  stabilize
  printObs ob

  setVar then_ 7
  stabilize
  printObs ob

  setVar flag False
  stabilize
  printObs ob

  setVar flag True
  setVar then_ 8
  setVar else_ 9
  stabilize
  printObs ob


-- "child ==> parent (in Top)"
-- "child --> parent (in Bind)"
-- t2[Var id=3] ==> b1[Bind id=4] <-- [Map2 id=7] (created on the fly)
--                                       ^   ^
--                                      /     \
--                         t1[Map id=2]        t3[Map id=6] (created on the fly)
--                                  ^              ^
--                                   \            /
--                                    v1[Var id=1]
--
-- TODO: Does it make sense to create and change value during a function on the rhs of bind?
-- say, is it leagal to write 'setVar t3' within rhs of [b1]?

testBind2 :: StateIO ()
testBind2 = do
  v1 <- createVar False (5 :: Int)
  t1 <- State.map (watch v1) (+ 10)
  t2 <- createVar False True

  -- (NodeRef a) -> (a -> StateIO (NodeRef b)) -> StateIO (NodeRef b)
  b1 <- bind (watch t2) (\_ -> do
                  t3 <- State.map (watch v1) (+ 20)
                  map2 t1 t3 (\x y -> x + y))
  -- b1 <- bind (watch t2) (\_ -> State.map (watch v1) (+ 20))
  ob <- createObserver b1
  stabilize
  printObs ob

  setVar v1 50
  -- when [v1] is changed, we should recompute [b1] directly and invalidate
  -- all the nodes created in rhs of [b1].
  stabilize
  printObs ob


verbose :: Bool
verbose = True

runTest :: StateIO a -> IO ()
runTest action = runStateT action initState >> return ()

