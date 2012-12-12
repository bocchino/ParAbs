module SML.ArraySlice

type 'a slice

exception Subscript

val slice : 'a array * int * int option -> 'a slice

val full : 'a array -> 'a slice

val subslice : 'a slice * int * int option -> 'a slice

val length : 'a slice -> int

val sub : 'a slice * int -> 'a

val update : 'a slice * int * 'a -> unit

