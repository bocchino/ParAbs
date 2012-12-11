module ArraySlice
type 'a slice
exception Subscript
val getArray : 'a slice -> 'a array
val getStart : 'a slice -> int
val getSize : 'a slice -> int
val checkRange : i:int * bound:int -> unit
val slice : arr:'a [] * i:int * sz:int option -> 'a slice
val full : arr:'a [] -> 'a slice
val subslice : sl:'a slice * i:int * sz:int option -> 'a slice
val length : sl:'a slice -> int
val sub : sl:'a slice * i:int -> 'a
val update : sl:'a slice * i:int * elt:'a -> unit

