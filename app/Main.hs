module Main where

import Incremental (Observer, Var, StateIO, NodeRef)
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
  putStrLnT "----------------- Begin example 1: Map -----------------"
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

example2 :: StateIO ()
example2 = do
  putStrLnT "----------------- Begin example 2: Map2 -----------------"
  v1 <- Inc.var (5 :: Int)
  v2 <- Inc.var 10
  n  <- Inc.map2 (+) (Inc.watch v1) (Inc.watch v2)
  ob <- Inc.observe n
  putStrLnT "All nodes are added"

  Inc.stabilize
  printObs ob
  putStrLnT "After first stabilization"

  Inc.writeVar v1 7
  -- setVar v2 11
  Inc.stabilize
  printObs ob
  putStrLnT "After second stabilization"


if_ :: Eq a
    => NodeRef Bool -> NodeRef a -> NodeRef a -> StateIO (NodeRef a)
if_ a b c = Inc.bind a (\x -> if x then return b else return c)

example3 :: StateIO ()
example3 = do
  putStrLnT "----------------- Begin example 3: Bind -----------------"
  flag   <- Inc.var True
  then_  <- Inc.var (5 :: Int)
  else_  <- Inc.var 6
  try_if <- if_ (Inc.watch flag) (Inc.watch then_) (Inc.watch else_)
  ob     <- Inc.observe try_if

  Inc.stabilize
  printObs ob

  Inc.writeVar then_ 7
  Inc.stabilize
  printObs ob

  Inc.writeVar flag False
  Inc.stabilize
  printObs ob

  Inc.writeVar flag True
  Inc.writeVar then_ 8
  Inc.writeVar else_ 9
  Inc.stabilize
  printObs ob


-- "child ==> parent (in Top)"
-- "child --> parent (in Bind)"
-- t2[Var id=3] ==> b1[Bind id=4] <-- [Map2 id=7] (created on the fly)
--                                       ^   ^
--                                      /     \
--                         t1[Map id=2]        t3[Map id=6] (created on the fly)
--                                  ^              ^
--                                   \            /
--                                    v1[Var id=1]
--
-- TODO: Does it make sense to create and change value during a function on the rhs of bind?
-- say, is it leagal to write 'setVar t3' within rhs of [b1]?

example4 :: StateIO ()
example4 = do
  putStrLnT "----------------- Begin example 4: Bind -----------------"
  v1 <- Inc.var (5 :: Int)
  t1 <- Inc.map (+ 10) (Inc.watch v1)
  t2 <- Inc.var True

  -- (NodeRef a) -> (a -> StateIO (NodeRef b)) -> StateIO (NodeRef b)
  b1 <- Inc.bind (Inc.watch t2) (\_ -> do
                  t3 <- Inc.map (+ 20) (Inc.watch v1)
                  Inc.map2 (\x y -> x + y) t1 t3)
  -- b1 <- bind (watch t2) (\_ -> State.map (watch v1) (+ 20))
  ob <- Inc.observe b1
  Inc.stabilize
  printObs ob

  Inc.writeVar v1 50
  -- when [v1] is changed, we should recompute [b1] directly and invalidate
  -- all the nodes created in rhs of [b1].
  Inc.stabilize
  printObs ob

main :: IO ()
main = mapM_ Inc.run
             [example1, example2, example3, example4]
