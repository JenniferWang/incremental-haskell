module Utils
  where

import Data.IORef
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict(get)
import Lens.Simple

import Types

pack :: (Eq a) => NodeRef a -> PackedNode
pack = PackedNode

readIORefT :: (IORef a) -> StateIO a
readIORefT = (lift . readIORef)

readNodeRef :: NodeRef a -> StateIO (Node a)
readNodeRef (Ref ref _ _) = readIORefT ref

modifyIORefT :: (IORef a) -> (a -> a) -> StateIO ()
modifyIORefT ref g = lift $ modifyIORef ref g

modifyNodeRef :: NodeRef a -> (Node a -> Node a) -> StateIO ()
modifyNodeRef (Ref ref _ _) = modifyIORefT ref

putStrLnT :: String -> StateIO ()
putStrLnT = (lift . putStrLn)

getStbNum:: StateIO Int
getStbNum = do s <- get; return $ s^.info.stbNum

getCurrScope :: StateIO Scope
getCurrScope = do s <- get; return $ s^.info.currScope


