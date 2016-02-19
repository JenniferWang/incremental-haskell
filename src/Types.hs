{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}

module Types
  where

import Data.IORef
import Lens.Simple

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
  , _pinfo :: ParentInfo a
  }

data ParentInfo a = ParentInfo {
    _numPar        :: Int
  , _par0          :: Maybe (IORef (Node a))
  , _par1AndBeyond :: [IORef (Node a)]
  }

---------------------------------- Var ----------------------------------------
data Var a = Var

---------------------------------- Packed_node --------------------------------
-- JaneStreet defines this for performance reason.


makeLenses ''Node
makeLenses ''ParentInfo

