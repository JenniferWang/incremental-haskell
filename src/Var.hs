module Var
  where
import Types
import Data.Maybe(fromJust, isNothing)

latestValue :: Var a -> a
latestValue var = let v0 = valueSetDuringStb var in
                      if isNothing v0 then (mvalue var)
                                      else (fromJust v0)


