module Point

exception UnequalLengths

type t
(* F# limitation *)
    = double list

val fromList : double list -> t
val fromArray : double array -> t
val toList : t -> double list
val toArray : t -> double array
val constant : double -> t
val zero : t
val one : t
val add : t * t -> t
val sub : t * t -> t
val adds : t * double -> t
val muls : t * double -> t
val divs : t * double -> t
val dot : t * t -> double
val toString : t -> string

