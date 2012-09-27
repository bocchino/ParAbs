(* Module that supports a pattern of updating a parallel array,
   while reading other data structures *)

signature ARRAY_UPDATER =
sig

    (* An updater consists of a mutable array parameterized by
       a value type 'a, and read-only state with type 'b *)
    (*shared*) type ('a, (*readonly*) 'b) updater

    (* Create a new updater from a fresh array, and read-only state *)
    val updater : (int * 'a) -> 'b -> ('a,'b) updater

    (* Update function: takes read-only state as input,
       and uses it to compute new values *)
    type ('a, (*readonly*) 'b) updateFn = 'b -> 'a -> 'a
		    
    (* Apply an update function in parallel to the array.  The
       update function can read the read-only state stored
       in the updater and update the array, but can not otherwise
       touch any global mutable state. The update function may
       allocate and use its own (local) mutable state. *)
    val update : ('a,'b) updater -> ('a,'b) updateFn -> unit

    (* Get the array out of the updater *)
    val getArray : ('a,'b) updater -> Array.array

end
