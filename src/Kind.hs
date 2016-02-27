module Kind
  where

import Lens.Simple
import Types

maxNumChildren :: Kind a -> Int
maxNumChildren (Const _)      = 0
maxNumChildren (Freeze _ _ _) = 1
maxNumChildren Invalid        = 0
maxNumChildren (Map _ _)      = 1
maxNumChildren Uninitialized  = 0
maxNumChildren (Variable _)   = 0


-- TODO: add error message
-- | 'slowGetChild' fetch the child at the given index.
slowGetChild :: (Eq a) => Kind a -> Index -> PackedNode
slowGetChild k index = (iteriChildren k (\_ n -> n)) !! index

-- | 'iteriChildren' iterate all the child node
iteriChildren :: (Eq a) => Kind a -> (Index -> PackedNode -> b) -> [b]
iteriChildren (Map _ n0_ref) g = [g 0 (PackedNode n0_ref)]
iteriChildren (Freeze _ child f) g = [g 0 (PackedNode child)]
iteriChildren _ _              = []

freeze_child_index :: Index
freeze_child_index = 0

isFreeze :: Kind a -> Bool
isFreeze (Freeze _ _ _) = True
isFreeze _              = False


