module Recompute_Heap where

import qualified Node as N
import Types
import Data.Heap


-----------------------------  Recompute Heap --------------------
-- Recompute heap holds the set of nodes that need to be computed. 
-- Used during stabilization to visit the nodes that need to be computed
-- in topological order, using the recompute heap to visit
-- them in increasing order of height
-- https://github.com/janestreet/incremental/blob/master/src/recompute_heap.mli

data RecomputeList = MinHeap Height PackedNode

link :: PackedNode -> RecomputeList -> RecomputeList
link (PackedNode node_ref) rec_list = do
          node <- readIORef node_ref  
          insert node^.heap.rec.heightInRecHeap node_ref rec_list
          

