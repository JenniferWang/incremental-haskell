{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}

module Types
  where

import Data.IORef
import Data.Unique
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
  , _par      :: ParentInfo
  , _height   :: Height      -- ^ used to visit nodes in topological order
  , _heap     :: HeapInfo
  , _handlers :: HandlersInfo a
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
    nid             :: Unique
  , _kind           :: Kind a
  , _forceNecessary :: Bool
  , _obsHead        :: Maybe DummyType -- the head of the doubly-linked list of observers
  , _value          :: ValueInfo a
  }

data HandlersInfo a = HandlersInfo {
    _numOnUpdates       :: Int
  , _isInHandleAfterStb :: Bool
  , _onUpdates          :: [DummyType2 a]
  }

data PackedNode = forall a. PackedNode (NodeRef a)

instance Eq PackedNode where
   (==) (PackedNode ref1) (PackedNode ref2) = ref1 == unsafeCoerce ref2

data ParentInfo  = ParentInfo {
    _numPar        :: Int
  , _par0          :: Maybe PackedNode
  , _par1AndBeyond :: [PackedNode]
  }

data HeapInfo = HeapInfo {
    _rec :: RecHeapInfo
  , _adj :: AdjHeapInfo
  }

data RecHeapInfo = RecHeapInfo {
    _heightInRecHeap :: Height
  , _prevInRecHeap   :: Maybe PackedNode
  , _nextInRecHeap   :: Maybe PackedNode
  }

data AdjHeapInfo = AdjHeapInfo {
    _heightInAdjHeap :: Height
  , _nextInAdjHeap   :: Maybe PackedNode
  }

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
data Status = Stabilizing
            | RunningOnUpdateHandlers
            | NotStabilizing
            | StabilizePreviouslyRaised

data StateInfo = StateInfo {
    _status       :: Status
  , _stb          :: StbInfo
  , _obs          :: ObsInfo
  , _setDuringStb :: DummyType
  , _num          :: NumberInfo
  }

type StateIO a = StateT StateInfo IO a

type StabilizationNum = Int

data BecameInfo = BecameInfo {
    _necessary   :: Int
  , _unnecessary :: Int
  }

data RecomputedInfo = RecomputedInfo {
    _byDefault                :: Int
  , _directlyBecauseOneChild  :: Int
  , _directlyBecauseMinHeight :: Int
  }

data NumberInfo = NumberInfo {
    _nodesBecame      :: BecameInfo
  , _nodesChanged     :: Int
  , _nodesCreated     :: Int
  , _nodesInvalidated :: Int
  , _nodesRecomputed  :: RecomputedInfo
  , _varSets          :: Int
  }

-- | 'StbInfo' stands for stablization information
data StbInfo = StbInfo {
    _stbNum    :: StabilizationNum
  , _currScope :: Scope
  , _recHeap   :: RecomputeHeap
  , _adjHeap   :: AdjustHeightsHeap
  }

-- | 'ObsInfo' stands for observer information
data ObsInfo = ObsInfo {
     _numActive  :: Int
   , _all        :: [DummyType]
   , _finalized  :: DummyType
   , _new        :: DummyType
   , _disallowed :: DummyType
   }

makeLenses ''Node
makeLenses ''ParentInfo
makeLenses ''StateInfo
makeLenses ''StbInfo
makeLenses ''ObsInfo
makeLenses ''HeapInfo
makeLenses ''RecHeapInfo
makeLenses ''AdjHeapInfo
makeLenses ''NodeInfo
makeLenses ''Freeze
makeLenses ''ValueInfo
makeLenses ''RecomputedInfo
makeLenses ''NumberInfo
makeLenses ''HandlersInfo
makeLenses ''BecameInfo
