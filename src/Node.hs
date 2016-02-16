module Node where

import Data.IORef
import Kind

data NodeId = NodeId { i :: IORef Int }

newId :: IO NodeId
newId = do
     fs <- newIORef 0
     return (NodeId {i = fs})

nextId :: NodeId -> IO Int
nextId nodeId = do
     id <- readIORef (i nodeId)
     writeIORef (i nodeId) (id + 1)
     return id
     
data Node = Node{
     nid :: Int,
     children :: [IORef Node]
 }
   

-- TO DO TYPE NODE = IORef Node ...

newNode :: NodeId -> IO Node
newNode nodeId = do 
       i <- nextId nodeId
       return (Node{nid = i, children = []})

showId :: Node -> Int
module Node where

import Data.IORef
import Kind

data NodeId = NodeId { i :: IORef Int }

newId :: IO NodeId
newId = do
     fs <- newIORef 0
     return (NodeId {i = fs})

nextId :: NodeId -> IO Int
nextId nodeId = do
     id <- readIORef (i nodeId)
     writeIORef (i nodeId) (id + 1)
     return id
     
data Node = Node{
     nid :: Int,
     children :: [IORef Node]
 }
   

-- TO DO TYPE NODE = IORef Node ...

newNode :: NodeId -> IO Node
newNode nodeId = do 
       i <- nextId nodeId
       return (Node{nid = i, children = []})

showId :: Node -> Int
showId n = nid n
showId n = nid n
