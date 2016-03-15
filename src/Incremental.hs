{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Incremental (
    observe
  , const
  , var
  , varInScope
  , map
  , map2
  , map3
  , map4
  , (>>|)
  , (>>=|)
  , freeze
  , freezeWhen
  , stabilize
  , amStabilizing
  , main
  ) where

import Prelude hiding (const, map)

import Types
import qualified Node as N
import qualified Var as V
import qualified State as S

const :: Eq a => a -> StateIO (NodeRef a)
const = S.const

observe :: Eq a => NodeRef a -> StateIO (Observer a)
observe = S.createObserver

map :: (Eq a, Eq b) => NodeRef b -> (b -> a) -> StateIO (NodeRef a)
map = S.map

map2 :: (Eq a, Eq b, Eq c) =>
     NodeRef b -> NodeRef c -> (b -> c -> a) -> StateIO (NodeRef a)
map2 = S.map2

map3 :: (Eq a, Eq b, Eq c, Eq d) =>
     NodeRef b
     -> NodeRef c
     -> NodeRef d
     -> (b -> c -> d -> a)
     -> StateIO (NodeRef a)
map3 = S.map3

map4 :: (Eq a, Eq b, Eq c, Eq d, Eq e) =>
     NodeRef b
     -> NodeRef c
     -> NodeRef d
     -> NodeRef e
     -> (b -> c -> d -> e -> a)
     -> StateIO (NodeRef a)
map4 = S.map4

bind :: (Eq a, Eq b) =>
     NodeRef a -> (a -> StateIO (NodeRef b)) -> StateIO (NodeRef b)
bind = S.bind

(>>|) :: (Eq a, Eq b) => NodeRef b -> (b -> a) -> StateIO (NodeRef a)
node >>| f = map node f

(>>=|) :: (Eq a, Eq b) =>
       NodeRef a -> (a -> StateIO (NodeRef b)) -> StateIO (NodeRef b)
node >>=| mf = bind node mf

-- By default, the create Var in global scope
var :: Eq a => a -> StateIO (Var a)
var = S.createVar False

-- This should only be used for nodes chreated in bind.
varInScope :: Eq a => a -> StateIO (Var a)
varInScope = S.createVar True

freeze :: Eq a => NodeRef a -> StateIO (NodeRef a)
freeze node = S.freeze node (\_ -> True)

freezeWhen :: Eq a => NodeRef a -> (a -> Bool) -> StateIO (NodeRef a)
freezeWhen = S.freeze

stabilize :: StateIO ()
stabilize = S.stabilize

amStabilizing :: StateIO Bool
amStabilizing = S.amStabilizing

-- TODO: delete, this is temporary
main :: IO ()
main = S.runTest S.testMap

