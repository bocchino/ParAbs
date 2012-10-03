(* Module that supports a pattern of modifying a parallel array,
   while reading other data structures *)

signature ARRAY_MODIFIER =
sig

    (* A modifier consists of a mutable array parameterized by
       a value type 'a, and read-only state with type 'b *)
    (*shared*) type ('a, (*readonly*) 'b) modifier

    (* Create a new modifier from a fresh array, and read-only state *)
    val modifier : (int * 'a) -> 'b -> ('a,'b) modifier

    (* Modify function: takes read-only state as input,
       and uses it to compute new values *)
    type ('a, (*readonly*) 'b) modifyFn = 'b -> 'a -> 'a
    type ('a, (*readonly*) 'b) modifyiFn = 'b -> (int * 'a) -> 'a
		    
    (* Apply a modify function in parallel to the array.  The
       modify function can read the read-only state stored
       in the modifier and modify the array, but can not otherwise
       touch any global mutable state. The modify function may
       allocate and use its own (local) mutable state. *)
    val modify : ('a,'b) modifier -> ('a,'b) modifyFn -> unit
    val modifyi : ('a,'b) modifier -> ('a,'b) modifyiFn -> unit

    (* Get the array out of the modifier *)
    val getArray : ('a,'b) modifier -> 'a Array.array

end
