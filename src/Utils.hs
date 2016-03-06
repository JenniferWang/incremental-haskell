module Utils
  where

import Data.IORef
import Types
import Control.Monad.Trans.Class

pack :: (Eq a) => NodeRef a -> PackedNode
pack = PackedNode

readIORefT :: (IORef a) -> StateIO a
readIORefT = (lift . readIORef)

readNodeRef :: NodeRef a -> StateIO (Node a)
readNodeRef (Ref ref _) = readIORefT ref

modifyIORefT :: (IORef a) -> (a -> a) -> StateIO ()
modifyIORefT ref g = lift $ modifyIORef ref g

modifyNodeRef :: NodeRef a -> (Node a -> Node a) -> StateIO ()
modifyNodeRef (Ref ref _) = modifyIORefT ref

putStrLnT :: String -> StateIO ()
putStrLnT = (lift . putStrLn)


