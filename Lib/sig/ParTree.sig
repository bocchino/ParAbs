signature PAR_TREE =
sig

    (* The type of a parallel tree with read/write privileges *)
    (*shared*) type 'a tree

    (* Type of a parallel tree with read-only privileges *)
    (*readonly*) type 'a readOnlyTree

    (* The type of a user-supplied index function that takes a value to insert, 
       a current value, and a parent value, and computes the index position for 
       continuing down the tree. *)
    type 'a indexFn = 'a * {current:'a,parent:'a option} -> int

    (* The type of a user-supplied reduction function that takes a
       current data value and a list of (child) data values, and produces
       a new data value. *)
    type 'a reduction = 'a * ('a list) -> 'a

    (* Construct a new empty tree with given arity *)
    val empty : int -> 'a tree

    (* Insert a value into the tree, using the given index function *)    
    val insert : 'a tree -> 'a -> 'a indexFn -> unit

    (* Apply the given reduction to the tree in parallel *)
    val reduce : 'a tree -> 'a reduction -> unit

    (* Obtain a read-only tree. *)
    val readOnly : 'a tree -> 'a readOnlyTree

    (* Get the value out of the root of a tree *)
    val getValue : 'a readOnlyTree -> 'a

    (* Get the nth child of the root of a tree *)
    val getChild : 'a readOnlyTree -> int -> ('a readOnlyTree) option

end
