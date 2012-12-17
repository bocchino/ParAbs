module RegionTree

(* F# forces us to expose this implementation *)
type 'a node = Inner of 'a inner
             | Leaf of 'a ref
and 'a inner = {data:'a option ref;
                children:'a node option array}
(* F# *)

(* The type of a user-supplied index function that takes a data value
   and tree level, and computes an index for continuing down the tree. *)
type 'a indexFn = 'a * int -> int

(* The type of a region tree with read/write privileges *)
(*shared*)
type 'a tree
(* F# forces us to expose this implementation *)
    = {arity:int;
       root:'a node option ref;
       indexFn:'a indexFn}

(* Type of a region tree with read-only privileges *)
(*readonly*)
type 'a readOnlyTree
(* F# forces us to expose this implementation *)
    = 'a tree

(*readonly*)
type 'a readOnlyNode
(* F# forces us to expose this implementation *)
    = 'a node

(* The type of a user-supplied reduction function that takes a
   current data value and a list of (child) data values, and produces
   a new data value. *)
type 'a reduction = 'a option -> 'a option list -> 'a option

(* Construct a new empty tree with given number of dimensions *)
val empty : int -> 'a indexFn -> 'a tree

(* Insert a value into the tree, using the given index function *)
val insert : 'a tree -> 'a -> unit

(* Apply the given reduction to the tree in parallel, and update
   the nodes with the results. *)
val reduce : 'a tree * int -> 'a reduction -> 'a option

(* Obtain a read-only tree. *)
val readOnly : 'a tree -> 'a readOnlyTree

(* Get the root node out of a tree *)
val getRoot : 'a readOnlyTree -> 'a readOnlyNode option

(* Get the value out of a node *)
val getData : 'a readOnlyNode option -> 'a option

(* Query whether a node is a leaf *)
val isLeaf : 'a readOnlyNode -> bool

(* Get the children of a node *)
val getChildren : 'a readOnlyNode -> 'a readOnlyNode option array option

