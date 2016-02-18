module Kind
  where

import Types
import Data.IORef
import Control.Monad(sequence)

maxNumChildren :: Kind a -> Int
maxNumChildren (Const _)     = 0
maxNumChildren Invalid       = 0
maxNumChildren (Map _ _)     = 1
maxNumChildren Uninitialized = 0
maxNumChildren (Variable _)       = 0


-- TODO: add error message
-- | 'slowGetChild' fetch the child at the given index.
slowGetChild :: Kind a -> Index -> IORef (Node a)
slowGetChild kind index = (iteriChildren kind (\_ n -> n)) !! index

-- | 'iteriChildren' iterate all the child node
iteriChildren :: Kind a -> (Index -> IORef (Node a) -> b) -> [b]
iteriChildren (Map _ n0_ref) g = [g 0 n0_ref]
iteriChildren _ _              = []

