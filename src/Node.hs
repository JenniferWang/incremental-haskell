module Node where

import Data.IORef
--import Kind

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

data Kind a =
    Const a
      | Invalid
 deriving Eq

data Node a = Node{
     nid      :: Int,
     kind     :: Kind a,
    -- value :: a,
     children :: [IORef (Node a)],
     parents  :: [IORef (Node a)]
 }
  deriving Eq
   

newNode :: NodeId -> IO (Node a)
newNode nodeId = do 
       i <- nextId nodeId
       return (Node{nid = i, kind = Invalid, children = [],parents=[]})


-- Checks whether n is a valid node
-- it is if and only if its kind is valid
isValid :: Node a -> Bool
isValid n = case kind n of 
         Invalid -> False
         _       -> True

-- Checks whether m is a child of n
isChild :: Eq a => Node a -> Node a -> IO Bool
isChild m n = isElem m (children n) 


-- Checks whether m is a parent of n
isParent :: Eq a => Node a -> Node a -> IO Bool
isParent m n = isElem m (parents n) 


----------------------   helper functions -----------------
-- test whether an element is present in a list of IORef a's
isElem :: Eq a => a -> [IORef a] -> IO Bool
isElem x [] = return False
isElem x (y:ys) = do 
               z <- readIORef y
               case z == x of
                True  -> return True
                False -> isElem x ys
