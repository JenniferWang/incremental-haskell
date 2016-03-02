module Var
  where

import Data.IORef
import Data.Maybe(fromJust, isNothing)

import Lens.Simple

import Types
import Utils

getLatestValue :: Var a -> StateIO a
getLatestValue (Var ref) = do
  n <- readIORefT (getRef ref)
  let v0 = valueSetDuringStb $ n^.kind
  if (isNothing v0) then return (mvalue $ n^.kind)
                    else return (fromJust v0)

getValue :: Var a -> StateIO a
getValue (Var ref) = do
  n <- readIORefT (getRef ref)
  return $ mvalue (n^.kind)




