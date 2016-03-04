module Kind
  where

import Types
import Utils

maxNumChildren :: Kind a -> Int
maxNumChildren (Const _)        = 0
maxNumChildren (Freeze _ _ _)   = 1
maxNumChildren Invalid          = 0
maxNumChildren Uninitialized    = 0
maxNumChildren (Variable _ _ _) = 0
maxNumChildren (Map _ _)        = 1
maxNumChildren (Map2 _ _ _)     = 2
maxNumChildren (Map3 _ _ _ _)   = 3
maxNumChildren (Map4 _ _ _ _ _) = 4


-- TODO: add error message
-- | 'slowGetChild' fetch the child at the given index.
slowGetChild :: (Eq a) => Kind a -> Index -> PackedNode
slowGetChild k index = (iteriChildren k (\_ n -> n)) !! index

-- | 'iteriChildren' iterate all the child node
iteriChildren :: (Eq a) => Kind a -> (Index -> PackedNode -> b) -> [b]
iteriChildren (Freeze _ child _) g = [ g 0 (pack child) ]
iteriChildren (Map _ b) g          = [ g 0 (pack b) ]
iteriChildren (Map2 _ b c) g       = [ g 0 (pack b), g 1 (pack c) ]
iteriChildren (Map3 _ b c d) g     = [ g 0 (pack b), g 1 (pack c), g 2 (pack d) ]
iteriChildren (Map4 _ b c d e) g   = [ g 0 (pack b), g 1 (pack c), g 2 (pack d)
                                     , g 3 (pack e) ]
iteriChildren _ _                  = []

freeze_child_index :: Index
freeze_child_index = 0

isFreeze :: Kind a -> Bool
isFreeze (Freeze _ _ _) = True
isFreeze _              = False


