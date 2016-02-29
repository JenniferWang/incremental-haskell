{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Incremental (
    hello
  ) where

import Types

import qualified Node as N
import qualified Var as V
import qualified RecomputeHeap as RH
import qualified State as S

hello :: IO ()
hello = putStrLn "Welcome to Incremental"
