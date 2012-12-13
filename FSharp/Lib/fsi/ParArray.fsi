module ParArray

(* The type of a parallel array with read/write privileges *)
(*shared*)
type 'a parArray
(* F# is broken *)
    = 'a array

(* Type of a parallel array with read-only privileges *)
(*readonly*)
type 'a readOnlyArray
(* F# is broken *)
    = 'a array

(* Create a new array *)
val array : int * 'a -> 'a parArray
val fromList : 'a list -> 'a parArray
val fromArray : 'a array -> 'a parArray

(* Obtain a read-only array *)
val readOnly : 'a parArray -> 'a readOnlyArray

(* Read-only operations *)
val length : 'a readOnlyArray -> int
val sub : 'a readOnlyArray * int -> 'a

(* Modify the array at one index position *)
val update : 'a parArray * int *'a -> unit

(* Modify the whole array in-place, in parallel *)
val modifyi : (int * 'a -> 'a) -> 'a parArray -> unit
val modify : ('a -> 'a) -> 'a parArray -> unit
