(* Module that supports a pattern of modifying a parallel array,
   while reading other data structures *)

module ArrayModifier

(* A modifier consists of a mutable array parameterized by
   a value type 'a, and read-only state with type 'b *)
(*mutable*)
type modifier<'a, (*readonly*) 'b> 
(* NOTE: Definition of modifier should not be visible here, but F# 
   compiler complains *)
  = 'a array * 'b 
(* END NOTE *)

(* Create a new modifier from a fresh array, and read-only state *)
val modifier : (int * 'a) -> 'b -> modifier<'a,'b>

(* Modify function: takes read-only state as input,
   and uses it to compute new values *)
type modifyFn<'a, (*readonly*) 'b>  = 'b -> 'a -> 'a
type modifyiFn<'a, (*readonly*) 'b> = 'b -> (int * 'a) -> 'a
                
(* Apply a modify function in parallel to the array.  The
   modify function can read the read-only state stored
   in the modifier and modify the array, but can not otherwise
   touch any global mutable state. The modify function may
   allocate and use its own (local) mutable state. *)
val modify : modifier<'a,'b> -> modifyFn<'a,'b> -> unit
val modifyi : modifier<'a,'b> -> modifyiFn<'a,'b> -> unit

(* Get the array out of the modifier *)
val getArray : modifier<'a,'b> -> 'a array


