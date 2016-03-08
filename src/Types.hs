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
    forall b. Eq b => ArrayFold {
        initAcc       :: a
      , foldFunc      :: a -> b -> a
      , childrenArray :: [NodeRef b]
      }
  | forall b. Eq b => Bind {
        func                :: b -> StateIO (NodeRef a)
      , lhs                 :: NodeRef b
      , rhs                 :: Maybe (NodeRef a)
      , nodesCreatedInScope :: [PackedNode]
      }
  | Const a
  | Freeze {
        mainNode       :: NodeRef a
      , childNode      :: NodeRef a
      , onlyFreezeWhen :: (a -> Bool)
      }
  | Invalid
  | Uninitialized
  | Variable {
        mvalue            :: a
      , valueSetDuringStb :: !(Maybe a)
      , setAt             :: StabilizationNum
      }
  | forall b. Eq b => Map (b -> a) (NodeRef b)
  | forall b c. (Eq b, Eq c) => Map2 (b -> c -> a) (NodeRef b) (NodeRef c)
  | forall b c d. (Eq b, Eq c, Eq d) =>
      Map3 (b ->c -> d -> a) (NodeRef b) (NodeRef c) (NodeRef d)
  | forall b c d e. (Eq b, Eq c, Eq d, Eq e) =>
      Map4 (b -> c -> d -> e -> a) (NodeRef b) (NodeRef c) (NodeRef d) (NodeRef e)

instance Show (Kind a) where
  show (ArrayFold _ _ _) = "Array_fold"
  show (Bind _ _ _ _)    = "Bind"
  show (Const _)         = "Const"
  show (Freeze _ _ _)    = "Freeze"
  show Invalid           = "Invalid"
  show (Map _ _)         = "Map"
  show (Map2 _ _ _)      = "Map2"
  show (Map3 _ _ _ _)    = "Map3"
  show (Map4 _ _ _ _ _)  = "Map4"
  show Uninitialized     = "Uninitialized"
  show (Variable _ _ _)  = "Var"

initKind :: Kind a
initKind = Uninitialized

---------------------------------- Node ---------------------------------------
-- 'Node' is the basic element in the DAG. 'NodeRef' contains a reference to a node
-- as well as a unique number to identify the node.
-- At first glance, it is a little strange not to store the ID in the node itself. We
-- do this because we want to make 'NodeRef a' and 'PackedNode' instance of 'Ord'
-- without unsafe coercion.
-- The node contains:
--   kind  : kind of the node, contains the pointer to child node
--   value : all value related information
--   edges :
--     parents : these are the real edge in the DAG. A [p] is added in the 'parents'
--               field of the [c] node iff the [p] node is necessary and the [c] node
--               is referred to by the [p] node in [p].kind
--               parents are recursively added in 'State.becameNecessary'.
--     obsOnNode : observers added to the current node
--   [temp]  : contains the teamporary information needed during computation.

data NodeRef a = Ref (IORef (Node a)) !Unique

instance Show (NodeRef a) where
  show (Ref _ index) = "[NodeRef ID = " ++ (show $ hashUnique index) ++ "]"

instance Eq (NodeRef a) where
  (==) (Ref _ i1) (Ref _ i2) = i1 == i2

instance Ord (NodeRef a) where
  (<=) (Ref _ i1) (Ref _ i2) = i1 <= i2

getID :: NodeRef a -> Unique
getID (Ref _ i) = i

getRef :: NodeRef a -> IORef (Node a)
getRef (Ref ref _) = ref

data Node a = Node {
    _kind      :: Kind a
  , _value     :: ValueInfo a
  , _edges     :: Edges
  , _createdIn :: Scope
  -- , _temp  :: Temp
  }

initNode :: Node a
initNode = Node initKind initValueInfo initEdges initScope

data Edges = Edges {
    _parents   :: Set PackedNode
  , _obsOnNode :: Set ObsID
  }

initEdges :: Edges
initEdges = Edges Set.empty Set.empty

-- data Temp = Temp {
--     _isInRecHeap :: !Bool
--   }

-- initTemp :: Temp
-- initTemp = Temp False

data ValueInfo a = ValueInfo {
    _v            :: Maybe a
  , _oldValue     :: Maybe a
  , _recomputedAt :: StabilizationNum
  , _changedAt    :: StabilizationNum
  }

instance Show a => Show (ValueInfo a) where
  show (ValueInfo v oldv rec change) =
    "value = " ++ show v ++ " " ++
    "oldValue = " ++ show oldv ++ " " ++
    "recomputedAt = " ++ show rec ++ " " ++
    "changedAt = " ++ show change

-- When the node is initialized, the value is **not** computed, thus
-- bose 'recomputedAt' and 'changedAt' is set to none
initValueInfo :: ValueInfo a
initValueInfo = ValueInfo Nothing Nothing none none

data PackedNode = forall a. Eq a => PackedNode (NodeRef a)

instance Eq PackedNode where
  (==) (PackedNode ref1) (PackedNode ref2) = (getID ref1) == (getID ref2)

instance Ord PackedNode where
  (<=) (PackedNode ref1) (PackedNode ref2) = (getID ref1) <= (getID ref2)

instance Show PackedNode where
  show (PackedNode ref) = "[PackedNode] =>" ++ show ref

---------------------------------- Var ----------------------------------------
-- Var class is a little bit different than in the JS library
-- Now, it is just a proxy and the information is stored directly in the node

newtype Var a = Var { watch :: NodeRef a}
instance Show (Var a) where
  show v = "[Var] watching " ++ show (watch v)

data PackedVar = forall a. Eq a => PackVar !(Var a)

---------------------------------- Bind --------------------------------------


---------------------------------- Scope --------------------------------------
data Scope = Top
           | forall a. Eq a => Bound (NodeRef a)

initScope :: Scope
initScope = Top

---------------------------------- Observers ----------------------------------
data ObsState = Created | InUse | Disallowed | Unlinked
  deriving (Show, Eq)

type ObsID = Unique
instance Show Unique where
  show u = show (hashUnique u)

data InterObserver a = InterObs {
    _obsID            :: !Unique
  , _state            :: !ObsState
  , _observing        :: !(NodeRef a)
  }

instance Show (InterObserver a) where
  show o = "[Observer ID = " ++ show (_obsID o)
            ++ " , state = " ++ (show $ _state o) ++ "]"

instance Eq (InterObserver a) where
  (==) o1 o2 = (_obsID o1) == (_obsID o2)

instance Ord (InterObserver a) where
  (<=) o1 o2 = (_obsID o1) <= (_obsID o2)

data PackedObs = forall a. Eq a => PackObs (InterObserver a)

getObsID :: PackedObs -> Unique
getObsID (PackObs o) = _obsID o

instance Show PackedObs where
  show (PackObs o) = "[PackedObs] =>" ++ show o

instance Eq PackedObs where
  (==) po1 po2 = (getObsID po1) == (getObsID po2)

instance Ord PackedObs where
  (<=) po1 po2 = (getObsID po1) <= (getObsID po2)

newtype Observer a = Obs ObsID

instance Show (Observer a) where
  show (Obs id) = "[Observer ID = " ++ show id ++ "]"

---------------------------------- State --------------------------------------
type StateIO a = StateT StateInfo IO a

data StateInfo = StateInfo {
    _info           :: StatusInfo
  , _recHeap        :: Set PackedNode
  , _observer       :: ObserverInfo
  , _varSetDuringStb :: [PackedVar]
  }

initState :: StateInfo
initState = StateInfo initStatusInfo Set.empty initObserverInfo []

data Status = Stabilizing
            | NotStabilizing
            | StabilizePreviouslyRaised

initStatus :: Status
initStatus= Stabilizing

data StatusInfo = StatusInfo {
    _status    :: Status
  , _stbNum    :: StabilizationNum
  , _currScope :: Scope
  , _debug     :: DebugInfo
  }

initStatusInfo :: StatusInfo
initStatusInfo = StatusInfo initStatus initStbNum initScope initDebugInfo

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
initStbNum = none + 1

none :: Int
none = -1

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
-- makeLenses ''Temp
makeLenses ''Edges
makeLenses ''StateInfo
makeLenses ''StatusInfo
makeLenses ''ObserverInfo
makeLenses ''ValueInfo
makeLenses ''RecomputedInfo
makeLenses ''BecameInfo
makeLenses ''DebugInfo
makeLenses ''InterObserver
