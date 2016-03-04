module Utils
  where

import Data.IORef
import Types
import Control.Monad.Trans.Class

pack :: (Eq a) => NodeRef a -> PackedNode
pack = PackedNode

readIORefT :: (IORef a) -> StateIO a
readIORefT = (lift . readIORef)

modifyIORefT :: (IORef a) -> (a -> a) -> StateIO ()
modifyIORefT ref g = lift $ modifyIORef ref g

putStrLnT :: String -> StateIO ()
putStrLnT = (lift . putStrLn)


