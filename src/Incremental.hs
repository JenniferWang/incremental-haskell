module Incremental (
    observe
  , disallowFutureUse
  , const
  , var
  , varInScope
  , readVar
  , writeVar
  , map
  , map2
  , map3
  , map4
  , bind
  , (>>|)
  , (>>=|)
  , freeze
  , freezeWhen
  , stabilize
  , amStabilizing
  , stabilizeAsync
  , waitForStb
  , readObs
  , run
  , Observer
  , Var(..)
  , NodeRef
  , StateIO
  ) where

import Control.Monad.Trans.State.Strict (runStateT)
import Prelude hiding (const, map)

import Types
import qualified Observer as O
import qualified Var      as V
import qualified State    as S

const :: Eq a => a -> StateIO (NodeRef a)
const = S.const

observe :: Eq a => NodeRef a -> StateIO (Observer a)
observe = S.createObserver

disallowFutureUse :: Eq a => Observer a -> StateIO ()
disallowFutureUse (Obs id) = do
  (PackObs inter_ob) <- O.getObsByID id
  S.disallowFutureUse inter_ob

map :: (Eq a, Eq b) => (b -> a) -> NodeRef b -> StateIO (NodeRef a)
map = S.map

map2 :: (Eq a, Eq b, Eq c) =>
     (b -> c -> a) -> NodeRef b -> NodeRef c  -> StateIO (NodeRef a)
map2 = S.map2

map3 :: (Eq a, Eq b, Eq c, Eq d) =>
     (b -> c -> d -> a)
     -> NodeRef b
     -> NodeRef c
     -> NodeRef d
     -> StateIO (NodeRef a)
map3 = S.map3

map4 :: (Eq a, Eq b, Eq c, Eq d, Eq e) =>
     (b -> c -> d -> e -> a)
     -> NodeRef b
     -> NodeRef c
     -> NodeRef d
     -> NodeRef e
     -> StateIO (NodeRef a)
map4 = S.map4

bind :: (Eq a, Eq b) =>
     StateIO (NodeRef a) -> (a -> StateIO (NodeRef b)) -> StateIO (NodeRef b)
bind = S.bind

(>>|) :: (Eq a, Eq b) => (b -> a) -> NodeRef b -> StateIO (NodeRef a)
f >>| node = map f node

(>>=|) :: (Eq a, Eq b) =>
       StateIO (NodeRef a) -> (a -> StateIO (NodeRef b)) -> StateIO (NodeRef b)
node >>=| mf = bind node mf

-- By default, the create Var in global scope
var :: Eq a => a -> StateIO (Var a)
var = S.createVar False

readVar :: Eq a => Var a -> StateIO a
readVar = V.getValue

writeVar :: Eq a => Var a -> a -> StateIO ()
writeVar = S.setVar

-- This should only be used for nodes chreated in bind.
varInScope :: Eq a => a -> StateIO (Var a)
varInScope = S.createVar True

freeze :: Eq a => NodeRef a -> StateIO (NodeRef a)
freeze node = S.freeze node (\_ -> True)

freezeWhen :: Eq a => NodeRef a -> (a -> Bool) -> StateIO (NodeRef a)
freezeWhen = S.freeze

-- This version of stabilization executes in one thread
stabilize :: StateIO ()
stabilize = S.stabilize

amStabilizing :: StateIO Bool
amStabilizing = S.amStabilizing

-- Execute stabilization asynchronously. This will be helpful if the computation
-- takes too long and the user interface should not be blocked by the computation.
-- Users could edit observers/set vars during stabilization.
stabilizeAsync :: StateIO ()
stabilizeAsync = S.stabilizeAsync

-- Wait for the async stabilization to finish before fetching the values
waitForStb :: StateIO ()
waitForStb = S.waitForStb

readObs :: (Eq a) => Observer a -> StateIO a
readObs = O.obsValueExn

run :: StateIO a -> IO ()
run action =  runStateT action initState >> return ()

