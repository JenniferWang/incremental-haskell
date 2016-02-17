module Incremental (
    hello
  ) where

import qualified Node as N
import qualified Var as V

hello :: IO ()
hello = putStrLn "Welcome to Incremental"
