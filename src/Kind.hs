module Kind
  where

import Data.Maybe (isNothing, fromJust)

import Types
import Utils

maxNumChildren :: Kind a -> Int
maxNumChildren (Bind _ _ _ _)   = 2
maxNumChildren (Const _)        = 0
maxNumChildren (Freeze _ _ _)   = 1
maxNumChildren Invalid          = 0
maxNumChildren Uninitialized    = 0
maxNumChildren (Variable _ _ _) = 0
maxNumChildren (Map _ _)        = 1
maxNumChildren (Map2 _ _ _)     = 2
maxNumChildren (Map3 _ _ _ _)   = 3
maxNumChildren (Map4 _ _ _ _ _) = 4


-- | 'slowGetChild' fetch the child at the given index.
slowGetChild :: (Eq a) => Kind a -> Index -> Maybe PackedNode
slowGetChild k index = if index < maxNumChildren k
                          then Just $ (iteriChildren k (\_ n -> n)) !! index
                          else Nothing

-- | 'iteriChildren' iterate all the child node
iteriChildren :: (Eq a) => Kind a -> (Index -> PackedNode -> b) -> [b]
iteriChildren (Freeze _ child _) g = [ g 0 (pack child) ]
iteriChildren (Map _ b) g          = [ g 0 (pack b) ]
iteriChildren (Map2 _ b c) g       = [ g 0 (pack b), g 1 (pack c) ]
iteriChildren (Map3 _ b c d) g     = [ g 0 (pack b), g 1 (pack c), g 2 (pack d) ]
iteriChildren (Map4 _ b c d e) g   = [ g 0 (pack b), g 1 (pack c), g 2 (pack d)
                                     , g 3 (pack e) ]
iteriChildren (Bind _ lhs rhs _) g = let l = g 0 (pack lhs)
                                      in if (isNothing rhs)
                                            then [l]
                                            else l:[g 1 (pack $ fromJust rhs)]
iteriChildren _ _                  = []

freeze_child_index :: Index
freeze_child_index = 0

isFreeze :: Kind a -> Bool
isFreeze (Freeze _ _ _) = True
isFreeze _              = False


