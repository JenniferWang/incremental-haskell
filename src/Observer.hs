module Observer
  where

import Control.Monad.Trans.State.Strict hiding (state)
import qualified Data.Map.Strict as Map
import Prelude hiding (all)
import Data.Maybe(isNothing, fromJust)
import Unsafe.Coerce

import Lens.Simple

import Types
import qualified Node as N

useIsAllowed :: InterObserver a -> Bool
useIsAllowed o = case (_state o) of Created    -> True
                                    InUse      -> True
                                    Disallowed -> False
                                    Unlinked   -> False

writeObsState :: ObsState -> PackedObs -> PackedObs
writeObsState new_state (PackObs o) = PackObs (o{_state = new_state})

------------------------------ StateIO Monad -------------------------------
obsValueExnInter :: Eq a => InterObserver a -> StateIO a
obsValueExnInter o = case (o^.state) of
  -- TODO: change to failwith
  Created -> error "obsValueExnInter: Called without stabilizing"
  InUse   -> do v <- N.getNodeValue (o^.observing)
                if (isNothing v)
                  then error "obsValueExnInter: Attempt to get value of an invalid node"
                  else return (fromJust v)
  _       -> error "obsValueExnInter: Called after disallowing future use"

obsValueExn :: Eq a => Observer a -> StateIO a
obsValueExn (Obs id) = do
  (PackObs ob) <- getObsByID id
  value        <- obsValueExnInter ob
  return $ unsafeCoerce value

unlinkFmObserving :: Eq a => InterObserver a -> StateIO ()
unlinkFmObserving ob = N.removeObs (ob^.observing) (ob^.obsID)

unlinkFmAll :: Eq a => InterObserver a -> StateIO ()
unlinkFmAll ob = modify (\s -> s & observer.all %~ (Map.delete $ ob^.obsID))

unlink :: Eq a => InterObserver a -> StateIO ()
unlink ob = unlinkFmObserving ob >> unlinkFmAll ob

unlinkP :: PackedObs -> StateIO ()
unlinkP (PackObs ob) = unlink ob

getObsByID :: ObsID -> StateIO PackedObs
getObsByID i = do
  env <- get
  return $ (env^.observer.all) Map.! i

modifyObs :: (PackedObs -> PackedObs) -> ObsID -> StateIO ()
modifyObs f i = modify (\s -> s & observer.all %~ (Map.adjust f i))

modifyObsState :: ObsID -> ObsState -> StateIO ()
modifyObsState i os = modifyObs (writeObsState os) i

