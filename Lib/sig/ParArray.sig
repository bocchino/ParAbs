signature PAR_ARRAY =
sig

    (* The type of a parallel array with read/write privileges *)
    (*shared*) type 'a array

    (* Type of a parallel array with read-only privileges *)
    (*readonly*) type 'a readOnlyArray

    (* Create a new array *)
    val array : int * 'a -> 'a array
    val fromList : 'a list -> 'a array

    (* Obtain a read-only array *)
    val readOnly : 'a array -> 'a readOnlyArray

    (* Read-only operations *)
    val length : 'a readOnlyArray -> int
    val sub : 'a readOnlyArray * int -> 'a

    (* Modify the array at one index position *)
    val update : 'a array * int *'a -> unit

    (* Modify the whole array in-place, in parallel *)
    val modifyi : (int * 'a -> 'a) -> 'a array -> unit
    val modify : ('a -> 'a) -> 'a array -> unit

end
