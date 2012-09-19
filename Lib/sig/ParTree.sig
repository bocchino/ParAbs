signature PAR_TREE =
sig

    (* The type of a parallel tree with read/write privileges *)
    (*shared*) type 'a tree

    (* Type of a parallel tree with read-only privileges *)
    (*readonly*) type 'a readOnlyTree
    (*readonly*) type 'a readOnlyNode

    (* The type of a user-supplied index function that takes a data value
       and tree level, and computes an index for continuing down the tree. *)
    type 'a indexFn = {data:'a,level:int} -> int

    (* The type of a user-supplied reduction function that takes a
       current data value and a list of (child) data values, and produces
       a new data value. *)
    type 'a reduction = 'a option -> 'a option list -> 'a option

    (* Construct a new empty tree with given arity *)
    val empty : int -> 'a indexFn -> 'a tree

    (* Insert a value into the tree, using the given index function *)    
    val insert : 'a tree -> 'a -> unit

    (* Apply the given reduction to the tree in parallel, and update
       the nodes with the results. *)
    val reduce : 'a tree -> 'a reduction -> 'a option

    (* Obtain a read-only tree. *)
    val readOnly : 'a tree -> 'a readOnlyTree

    (* Get the root node out of a tree *)
    val getRoot : 'a readOnlyTree -> 'a readOnlyNode option

    (* Get the value out of a node *)
    val getData : 'a readOnlyNode -> 'a option

    (* Query whether a node is a leaf *)
    val isLeaf : 'a readOnlyNode -> bool

    (* Get the nth child of the root of a tree *)
    val getChild : 'a readOnlyNode -> int -> 'a readOnlyNode option

end
