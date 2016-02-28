## Right now we don't support `onUpdateHandler`

## State transformer `StateIO`
```
type StateIO a = StateT StateInfo IO a
```
## Break down large record using `Lens`
The global state we want to maitain contains many fields. It is not efficient for Haskell to update records with many fields (takes O(n)). One solution is to mark each field with `IORef` and use `modifyIORfe`. This smells like imperative code and makes code complicated. Another solution is to organize fields in a hierarchical manner which takes O(log(n)/log(m)), with approximately m fields in each record. We use `Simple.Lens` library to simplify looking up/ updating a field in this nested
structure.

## When should we use `IORef` and when not?
(Incremental is base on directed acyclic graph (DAG); however, it is not straight forward to define a graph in a 'node-to-node' style in Haskell. Why? What's the difference between a tree and a graph? The canonical answer is : graph is not algebraic. But this is hard to understand (for me). My understanding is that graphs have back edges! This makes it hard to mutate a node. Say I would like to mutate `n1`. If it's a tree, `n1` can have no more than one parent. Thus, I could trace down from root and change the nodes along the path to `n1` as well as the whole subtree rooted by `n1` (compiler is smart enough to do far less work to achieve this). Now, suppose `n1` have several parents, how can we **efficiently** update `n1` such that the mutation is visible to all nodes pointing to `n1`?!)

Our remedy to this problem is to use `IORef` (like pointers in C) for nodes. In `Node`, we define a field called `parent`, which is an array of `IORef (Parent_node)`. There is also a data type called `Kind`.  `Node` and `Kind` are like a couple in the original design where `Kind` holds the information of value and pointer to children while `Node` holds the pointer to parents, to its `Kind` and all the other information necessary to do the recomputation. 

Note that `Node` has far more fields than `Kind`, which could be expensive to 'mutate'. By mutate, we mean something like this 
```
setKind :: IORef (Node a) -> Kind a -> IO ()
setKind n_ref k = modifyIORef' n_ref (\n -> n{kind = k})
```
`setKind` will update the kind in a node given the node reference. The mutation is visible to all the nodes pointing to it. However, we might need to pay for creating a new `Node` through `\n -> n{kind = k}`. We make the mutation visible to other nodes by changing the content pointed by the `IORef`, but the content has been replaced by a new `Node` record created on the fly. Will the compiler do optimization on this? Well, one could probably use `IORef` for **all** the fields in the record. But this doesn't make much sense. This will add a layer of indirection. The code is already clumsy with all `IORef (Node a)` being passed around and adding another layer of `IORef` can only make things worse.

We could definitely implement the graph as an adjacency list, but as we need to update the graph frequently, we will still need IO monad...

## `Packed_node`
[link](https://github.com/janestreet/incremental/blob/master/src/node.ml#L46)
```
  ; mutable parent1_and_beyond                : Packed_node.t Uopt.t array
  ; mutable parent0                           : Packed_node.t Uopt.t
```
Convert node of different types to a single type. In this way, we could have list of heterogeneous nodes.

## Is this scalable? Multi-threaded?

## Memo 
* The OCaml library creates different graphs by instantiating the module system. In the Haskell version, we create different graphs by using differnt IO monad threads.

