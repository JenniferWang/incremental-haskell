module Recompute_Heap where

import Types

import Data.Heap (Heap,Entry(..))
import qualified Data.Heap as H
import Data.IORef

import Lens.Simple

-----------------------------  Recompute Heap --------------------
-- Recompute heap holds the set of nodes that need to be computed. 
-- Used during stabilization to visit the nodes that need to be computed
-- in topological order, using the recompute heap to visit
-- them in increasing order of height
-- https://github.com/janestreet/incremental/blob/master/src/recompute_heap.mli


-- | length of the heap
length :: RecomputeHeap -> Int
length = H.size 

-- | add a node to heap
add :: PackedNode -> RecomputeHeap -> IO RecomputeHeap
add (PackedNode node_ref) rec_heap = do
          n <- readIORef node_ref  
          return (H.insert (Entry (n^.height) (PackedNode node_ref)) rec_heap)

-- | removes a node from heap
remove :: PackedNode -> RecomputeHeap -> RecomputeHeap
remove packed_node rec_heap = H.filter (\(Entry _ n) -> (packed_node == n)) rec_heap

-- | removes and returns a node in heap with minimum height
--   if the heap is not empty
pop :: RecomputeHeap -> Maybe (PackedNode, RecomputeHeap)
pop rec_heap = case H.uncons rec_heap of 
          Just ((Entry _ packed_node),heap) -> Just (packed_node, heap)
          Nothing                           -> Nothing

