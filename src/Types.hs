{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}

module Types
  where

import Data.IORef
import Data.Unique
import Data.Heap (Heap,Entry(..))
import Control.Monad.Trans.State.Strict hiding (State)

import Lens.Simple

type Index = Int
type Height = Int

---------------------------------- Kind ---------------------------------------
-- Kind stores the computational information of each node.
-- https://github.com/janestreet/incremental/blob/master/src/kind.mli

-- Here I didn't use a IORef for Node because none of the methods in this module
-- will cause mutation
data Kind a =
    Const a
  | FreezeNode (Freeze a)
  | Invalid
  | forall b. Map (b -> a) (NodeRef a) -- TODO: not sure about this
  | Uninitialized
  | Variable (Var a)

instance Show (Kind a) where
  show (Const _)     = "Const"
  show (FreezeNode _)    = "Freeze"
  show Invalid       = "Invalid"
  show (Map _ _)     = "Map"
  show Uninitialized = "Uninitialized"
  show (Variable _)  = "Var"

---------------------------------- Freeze -------------------------------------
-- | An [Freeze a] is a kind of DAG node that takes on the value of another node
-- and doesn't change thereafter.

data Freeze a = Freeze {
    _mainNode       :: NodeRef a
  , _childNode      :: NodeRef a
  , _onlyFreezeWhen :: a -> Bool
  }

---------------------------------- Node ---------------------------------------
data NodeRef a = Ref (IORef (Node a)) !Unique

instance Show (NodeRef a) where
  show (Ref _ index) = show $ hashUnique index

getID :: NodeRef a -> Unique
getID (Ref ref id) = id

getRef :: NodeRef a -> IORef (Node a)
getRef (Ref ref id) = ref

data Node a = Node {
    _node     :: NodeInfo a
  , _parents  :: [PackedNode]
  , _height   :: Height      -- ^ used to visit nodes in topological order
  , _handlers :: HandlersInfo a
  , _obsHead  :: Maybe DummyType -- the head of the doubly-linked list of observers
  }

-- | 'ValueInfo'
-- [_v]
--   starts as Nothing, the first time [Node a] is computed it is set to [some]
--   and remains [some] thereafter, until [Node a]  is invalidated, if ever.
-- [_recomputedAt]
--   the last stabilization when node val is computed
-- [_changedAt]
--    the last stabilization when this node was computed and not cut off. It is
--    used to detect the node's parents are stale and need to be recomputed
data ValueInfo a = ValueInfo {
    _v            :: Maybe a
  , _recomputedAt :: StabilizationNum
  , _changedAt    :: StabilizationNum
  }

data NodeInfo a = NodeInfo {
    _kind   :: Kind a
  , _value  :: ValueInfo a
  , _numPar :: !Int
  }

data HandlersInfo a = HandlersInfo {
    _numOnUpdates       :: Int
  , _isInHandleAfterStb :: Bool
  , _onUpdates          :: [DummyType2 a]
  }

data PackedNode = forall a. PackedNode (NodeRef a)

instance Eq PackedNode where
  (==) (PackedNode ref1) (PackedNode ref2) = (getID ref1) == (getID ref2)

instance Ord PackedNode where
  (<=) (PackedNode ref1) (PackedNode ref2) = (getID ref1) <= (getID ref2)

instance Show PackedNode where
  show (PackedNode ref) = show ref
---------------------------------- Var ----------------------------------------
data Var a = Var

---------------------------------- Scope --------------------------------------
data Scope = Scope

---------------------------------- Heap ---------------------------------------
type RecomputeHeap = Heap (Entry Height PackedNode)

initRecHeap :: RecomputeHeap
-- TODO
initRecHeap = undefined

data AdjustHeightsHeap = AdjustHeightsHeap

initAdjHeap :: AdjustHeightsHeap
initAdjHeap = AdjustHeightsHeap

---------------------------------- Observers ----------------------------------

data DummyType = DummyType

data DummyType2 a = DummyType2

---------------------------------- State --------------------------------------
type StateIO a = StateT StateInfo IO a

data StateInfo = StateInfo {
    _info           :: StatusInfo
  , _recHeap        :: RecomputeHeap
  , _adjHeap        :: AdjustHeightsHeap
  , _handleAfterStb :: [PackedNode]
  , _observer       :: ObserverInfo
  }

initState :: StateInfo
initState = StateInfo initStatusInfo initRecHeap initAdjHeap [] initObserverInfo

data Status = Stabilizing
            | RunningOnUpdateHandlers
            | NotStabilizing
            | StabilizePreviouslyRaised

initStatus :: Status
initStatus= Stabilizing

data StatusInfo = StatusInfo {
    _status :: Status
  , _stbNum :: StabilizationNum
  , _debug  :: DebugInfo
  }

initStatusInfo :: StatusInfo
initStatusInfo = StatusInfo initStatus initStbNum initDebugInfo

-- | 'ObsInfo' stands for observer information
data ObserverInfo = ObserverInfo {
     _numActive  :: Int
   , _all        :: [DummyType]
   , _finalized  :: [DummyType]
   , _new        :: [DummyType]
   , _disallowed :: [DummyType]
   }

initObserverInfo :: ObserverInfo
initObserverInfo = ObserverInfo 0 [] [] [] []

type StabilizationNum = Int
initStbNum :: StabilizationNum
initStbNum = 0

---- These fields are for debugging and profiling
data DebugInfo = DebugInfo {
    _nodesBecame      :: BecameInfo
  , _nodesChanged     :: Int
  , _nodesCreated     :: Int
  , _nodesInvalidated :: Int
  , _nodesRecomputed  :: RecomputedInfo
  , _varSets          :: Int
  } deriving (Show)

initDebugInfo :: DebugInfo
initDebugInfo = DebugInfo initBecameInfo 0 0 0 initRecomputedInfo 0

data BecameInfo = BecameInfo {
    _necessary   :: Int
  , _unnecessary :: Int
  } deriving (Show)

initBecameInfo :: BecameInfo
initBecameInfo = BecameInfo 0 0

data RecomputedInfo = RecomputedInfo {
    _byDefault                :: Int
  , _directlyBecauseOneChild  :: Int
  , _directlyBecauseMinHeight :: Int
  } deriving (Show)

initRecomputedInfo :: RecomputedInfo
initRecomputedInfo = RecomputedInfo 0 0 0

makeLenses ''Node
makeLenses ''StateInfo
makeLenses ''StatusInfo
makeLenses ''ObserverInfo
makeLenses ''NodeInfo
makeLenses ''Freeze
makeLenses ''ValueInfo
makeLenses ''RecomputedInfo
makeLenses ''BecameInfo
makeLenses ''HandlersInfo
makeLenses ''DebugInfo
