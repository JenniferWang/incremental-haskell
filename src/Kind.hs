module Kind
  where

import Types
import Data.IORef

maxNumChildren :: Kind a -> Int
maxNumChildren (Const _)     = 0
maxNumChildren Invalid       = 0
maxNumChildren (Map _ _)     = 1
maxNumChildren Uninitialized = 0
maxNumChildren (Variable _)       = 0


-- TODO: add error message
-- | 'slowGetChild' fetch the child at the given index.
slowGetChild :: Kind a -> Index -> PackedNode
slowGetChild k index = (iteriChildren k (\_ n -> n)) !! index

-- | 'iteriChildren' iterate all the child node
iteriChildren :: Kind a -> (Index -> PackedNode -> b) -> [b]
iteriChildren (Map _ n0_ref) g = [g 0 (PackedNode n0_ref)]
iteriChildren _ _              = []

