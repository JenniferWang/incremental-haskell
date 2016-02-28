{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}

module Types
  where

import Data.IORef
import Data.Unique
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Heap (Heap,Entry(..))
import Control.Monad.Trans.State.Strict hiding (State)
import Prelude hiding (all)

import Lens.Simple

type Index = Int
type Height = Int

initHeight :: Height
initHeight = 0

---------------------------------- Kind ---------------------------------------
-- Kind stores the computational information of each node.
-- https://github.com/janestreet/incremental/blob/master/src/kind.mli

-- When you add a new kind, please update the following functions:
--   Kind.iteriChildren
--   Kind.maxNumChildren
--   Node.shouldBeInvalidated
--
-- An [Freeze a] is a kind of DAG node that takes on the value of another node
-- and doesn't change thereafter.

data Kind a =
    Const a
  | Freeze { mainNode       :: NodeRef a
           , childNode      :: NodeRef a
           , onlyFreezeWhen :: (a -> Bool)
           }
  | Invalid
  | forall b. Eq b => Map (b -> a) (NodeRef b)
  | Uninitialized
  | Variable (Var a)

instance Show (Kind a) where
  show (Const _)      = "Const"
  show (Freeze _ _ _) = "Freeze"
  show Invalid        = "Invalid"
  show (Map _ _)      = "Map"
  show Uninitialized  = "Uninitialized"
  show (Variable _)   = "Var"

initKind :: Kind a
initKind = Uninitialized

---------------------------------- Node ---------------------------------------
data NodeRef a = Ref (IORef (Node a)) !Unique

instance Show (NodeRef a) where
  show (Ref _ index) = "[NodeRef ID = " ++ (show $ hashUnique index) ++ " ]"

instance Eq (NodeRef a) where
  (==) (Ref _ i1) (Ref _ i2) = i1 == i2

instance Ord (NodeRef a) where
  (<=) (Ref _ i1) (Ref _ i2) = i1 <= i2

getID :: NodeRef a -> Unique
getID (Ref ref id) = id

getRef :: NodeRef a -> IORef (Node a)
getRef (Ref ref id) = ref

data Node a = Node {
    _node     :: NodeInfo a
  , _parents  :: Set PackedNode
  , _height   :: Height      -- ^ used to visit nodes in topological order
  , _obsOnNode :: Set ObsID -- ^ set of observer ID
  }

initNode :: Node a
initNode = Node initNodeInfo Set.empty initHeight  Set.empty

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
  , _oldValue     :: Maybe a
  , _recomputedAt :: StabilizationNum
  , _changedAt    :: StabilizationNum
  }

initValueInfo :: ValueInfo a
initValueInfo = ValueInfo Nothing Nothing initStbNum initStbNum

data NodeInfo a = NodeInfo {
    _kind   :: Kind a
  , _value  :: ValueInfo a
  , _numPar :: !Int
  }

initNodeInfo :: NodeInfo a
initNodeInfo = NodeInfo initKind initValueInfo 0

data PackedNode = forall a. Eq a => PackedNode (NodeRef a)

instance Eq PackedNode where
  (==) (PackedNode ref1) (PackedNode ref2) = (getID ref1) == (getID ref2)

instance Ord PackedNode where
  (<=) (PackedNode ref1) (PackedNode ref2) = (getID ref1) <= (getID ref2)

instance Show PackedNode where
  show (PackedNode ref) = "[PackedNode] =>" ++ show ref
---------------------------------- Var ----------------------------------------
data Var a = Var {
    mvalue            :: !a
  , valueSetDuringStb :: !(Maybe a)
  , setAt             :: StabilizationNum
  , watch             :: NodeRef a
  }

---------------------------------- Scope --------------------------------------
data Scope = Scope

---------------------------------- Heap ---------------------------------------
type RecomputeHeap = Heap (Entry Height PackedNode)

initRecHeap :: RecomputeHeap
-- TODO
initRecHeap = undefined

---------------------------------- Observers ----------------------------------
data ObsState = Created | InUse | Disallowed | Unlinked
  deriving (Show, Eq)

type ObsID = Unique

data Observer a = Obs {
    _obsID            :: !Unique
  , _state            :: !ObsState
  , _observing        :: !(NodeRef a)
  }

instance Show (Observer a) where
  show o = "[Observer ID = " ++ show (hashUnique $ _obsID o)
            ++ " , state = " ++ (show $ _state o) ++ "]"

instance Eq (Observer a) where
  (==) o1 o2 = (_obsID o1) == (_obsID o2)

instance Ord (Observer a) where
  (<=) o1 o2 = (_obsID o1) <= (_obsID o2)

data PackedObs = forall a. Eq a => PackedObs (Observer a)

getObsID :: PackedObs -> Unique
getObsID (PackedObs o) = _obsID o

instance Show PackedObs where
  show (PackedObs o) = "[PackedObs] =>" ++ show o

instance Eq PackedObs where
  (==) po1 po2 = (getObsID po1) == (getObsID po2)

instance Ord PackedObs where
  (<=) po1 po2 = (getObsID po1) <= (getObsID po2)

data DummyType = DummyType
data DummyType2 a = DummyType2

---------------------------------- State --------------------------------------
type StateIO a = StateT StateInfo IO a

data StateInfo = StateInfo {
    _info           :: StatusInfo
  , _recHeap        :: RecomputeHeap
  , _observer       :: ObserverInfo
  }

initState :: StateInfo
initState = StateInfo initStatusInfo initRecHeap initObserverInfo

data Status = Stabilizing
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

-- | The actual information of observers are stored in [_all]
-- In either [Node._obsOnNode] ore here, we store the [ObsID]
data ObserverInfo = ObserverInfo {
     _numActive  :: Int
   , _all        :: Map ObsID PackedObs
   , _new        :: [PackedObs]
   , _disallowed :: [ObsID]
   }

initObserverInfo :: ObserverInfo
initObserverInfo = ObserverInfo 0 Map.empty [] []

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
makeLenses ''ValueInfo
makeLenses ''RecomputedInfo
makeLenses ''BecameInfo
makeLenses ''DebugInfo
makeLenses ''Observer
