module Main where

import Incremental (Observer, Var, StateIO)
import qualified Incremental as Inc
import Control.Monad.Trans.Class (lift)

putStrLnT :: String -> StateIO ()
putStrLnT = (lift . putStrLn)

printObs :: (Show a, Eq a) => Observer a -> StateIO ()
printObs obs = Inc.readObs obs >>= \x -> putStrLnT $ show obs ++ " = " ++ show x

printVar :: (Show a, Eq a) => Var a -> StateIO ()
printVar x = Inc.readVar x
          >>= \y -> putStrLnT $ show x ++ " = " ++ show y

example1 :: StateIO ()
example1 = do
  v1    <- Inc.var (5 :: Int)
  v2    <- Inc.map (+ 6) (Inc.watch v1)
  -- obs   <- createObserver v2
  putStrLnT "All nodes are added"

  Inc.stabilize
  printVar v1
  -- printObs obs

  Inc.writeVar v1 10
  Inc.stabilize
  printVar v1
  -- printObs obs

main :: IO ()
main = Inc.run example1
