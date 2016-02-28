module Observer
  where

import Control.Monad.Trans.State.Strict hiding (state)
import qualified Data.Map.Strict as Map
import Prelude hiding (all)
import Data.Maybe(isNothing, fromJust)

import Lens.Simple

import Types
import qualified Node as N

useIsAllowed :: Observer a -> Bool
useIsAllowed o = case (_state o) of Created    -> True
                                    InUse      -> True
                                    Disallowed -> False
                                    Unlinked   -> False

------------------------------ StateIO Monad -------------------------------

obsValueExn :: Eq a => Observer a -> StateIO a
obsValueExn o = case (o^.state) of
                  -- TODO: change to failwith
                  Created -> error "State.obsValueExn called without stabilizing"
                  InUse   -> do v <- N.getNodeValue (o^.observing)
                                if (isNothing v)
                                   then error "Attempt to get value of an invalid node"
                                   else return (fromJust v)
                  _       -> error "State.obsValueExn called after disallowing future use"

unlinkFmObserving :: Eq a => Observer a -> StateIO ()
unlinkFmObserving ob = N.removeObs (ob^.observing) (ob^.obsID)

unlinkFmAll :: Eq a => Observer a -> StateIO ()
unlinkFmAll ob = modify (\s -> s & observer.all %~ (Map.delete $ ob^.obsID))

