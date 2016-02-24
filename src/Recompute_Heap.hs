module Recompute_Heap where

import qualified Node as N
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

type RecomputeList = Heap (Entry Height PackedNode)

link :: PackedNode -> RecomputeList -> IO RecomputeList
link (PackedNode node_ref) rec_list = do
          node <- readIORef node_ref  
          return (H.insert (Entry (node^.height) (PackedNode node_ref)) rec_list)
          

