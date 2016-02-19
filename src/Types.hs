{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}

module Types
  where

import Data.IORef
import Lens.Simple
import Unsafe.Coerce

type Index = Int

---------------------------------- Kind ---------------------------------------
-- Kind stores the computational information of each node.
-- https://github.com/janestreet/incremental/blob/master/src/kind.mli

-- Here I didn't use a IORef for Node because none of the methods in this module
-- will cause mutation
data Kind a =
    Const a
  | Invalid
  | forall b. Map (b -> a) (IORef (Node a)) -- TODO: not sure about this
  | Uninitialized
  | Variable (Var a)

instance Show (Kind a) where
  show (Const _)     = "Const"
  show Invalid       = "Invalid"
  show (Map _ _)     = "Map"
  show Uninitialized = "Uninitialized"
  show (Variable _)  = "Var"


---------------------------------- Node ---------------------------------------
data Node a = Node {
    nid    :: Int
  , _kind  :: Kind a
    -- value :: a,
  , _pinfo :: ParentInfo 
  }

data PackedNode = forall a. PackedNode (IORef (Node a))

instance Eq PackedNode where
   (==) (PackedNode ref1) (PackedNode ref2) = ref1 == unsafeCoerce ref2

data ParentInfo  = ParentInfo {
    _numPar        :: Int
  , _par0          :: Maybe PackedNode
  , _par1AndBeyond :: [PackedNode]
  }

---------------------------------- Var ----------------------------------------
data Var a = Var

---------------------------------- Packed_node --------------------------------
-- JaneStreet defines this for performance reason.


makeLenses ''Node
makeLenses ''ParentInfo

