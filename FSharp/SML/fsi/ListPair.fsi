module SML.ListPair

val zipEq : 'a list * 'b list -> ('a * 'b) list

val mapEq : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
