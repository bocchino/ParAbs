signature POINT =
sig
    type t
    val fromList : real list -> t
    val fromArray : real array -> t
    val toList : t -> real list
    val toArray : t -> real array
    val const : real -> t
    val zero : t
    val one : t
    val add : t * t -> t
    val sub : t * t -> t
    val adds : t * real -> t
    val muls : t * real -> t
    val divs : t * real -> t
    val dot : t * t -> real
    val toString : t -> string
end
