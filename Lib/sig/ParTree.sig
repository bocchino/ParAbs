signature PAR_TREE =
sig

    (* The type of a parallel tree with read/write privileges *)
    (*shared*) type 'data_t tree

    (* Type of a parallel tree with read-only privileges *)
    (*readonly*) type 'data_t readOnlyTree

    (* The type of a user-supplied index function that takes a value to insert, 
       a current value, and a parent value, and computes the index position for 
       continuing down the tree. *)
    type 'data_t indexFn = 'data_t * {current:'data_t,parent:'data_t option} -> int

    (* The type of a user-supplied reduction function that takes a
       current data value and a list of (child) data values, and produces
       a new data value. *)
    type 'data_t reduction = 'data_t * ('data_t list) -> 'data_t

    (* Construct a new empty tree with given arity *)
    val empty : int -> 'data_t tree

    (* Insert a value into the tree, using the given index function *)    
    val insert : 'data_t tree -> 'data_t -> 'data_t indexFn -> unit

    (* Apply the given reduction to the tree *)
    val reduce : 'data_t tree -> 'data_t reduction -> unit

    (* Obtain a read-only tree. *)
    val readOnly : 'data_t tree -> 'data_t readOnlyTree

    (* Get the value out of the root of a tree *)
    val getValue : 'data_t readOnlyTree -> 'data_t

    (* Get the nth child of the root of a tree *)
    val getChild : 'data_t readOnlyTree -> int -> ('data_t readOnlyTree) option

end
