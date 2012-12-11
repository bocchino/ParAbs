module ArraySlice

type 'a slice
exception Subscript

val slice : arr:'a [] * i:int * sz:int option -> 'a slice
val full : arr:'a [] -> 'a slice
val subslice : sl:'a slice * i:int * sz:int option -> 'a slice
val length : sl:'a slice -> int
val sub : sl:'a slice * i:int -> 'a
val update : sl:'a slice * i:int * elt:'a -> unit

