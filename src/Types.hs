{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}

module Types
  where

import Data.IORef
import Data.Unique
import Data.Heap (Heap,Entry(..))
import Unsafe.Coerce
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
type NodeRef a = IORef (Node a)

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
    _nid    :: Unique
  , _kind   :: Kind a
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
   (==) (PackedNode ref1) (PackedNode ref2) = ref1 == unsafeCoerce ref2

---------------------------------- Var ----------------------------------------
data Var a = Var

---------------------------------- Scope --------------------------------------
data Scope = Scope

---------------------------------- Heap ---------------------------------------
data RecomputeHeap = RecomputeHeap

data AdjustHeightsHeap = AdjustHeightsHeap

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

data Status = Stabilizing
            | RunningOnUpdateHandlers
            | NotStabilizing
            | StabilizePreviouslyRaised

data StatusInfo = StatusInfo {
    _status :: Status
  , _stbNum :: StabilizationNum
  , _debug  :: DebugInfo
  }

-- | 'ObsInfo' stands for observer information
data ObserverInfo = ObserverInfo {
     _numActive  :: Int
   , _all        :: [DummyType]
   , _finalized  :: [DummyType]
   , _new        :: [DummyType]
   , _disallowed :: [DummyType]
   }

type StabilizationNum = Int

---- These fields are for debugging and profiling
data DebugInfo = DebugInfo {
    _nodesBecame      :: BecameInfo
  , _nodesChanged     :: Int
  , _nodesCreated     :: Int
  , _nodesInvalidated :: Int
  , _nodesRecomputed  :: RecomputedInfo
  , _varSets          :: Int
  } deriving (Show)

data BecameInfo = BecameInfo {
    _necessary   :: Int
  , _unnecessary :: Int
  } deriving (Show)

data RecomputedInfo = RecomputedInfo {
    _byDefault                :: Int
  , _directlyBecauseOneChild  :: Int
  , _directlyBecauseMinHeight :: Int
  } deriving (Show)


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
