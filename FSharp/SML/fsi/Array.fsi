module SML.Array

val array : int * 'a -> 'a array

val sub : 'a array * int -> 'a

val update : 'a array * int * 'a  -> unit

val foldl : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b

val foldr : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b

val fromList : 'a list -> 'a array

val app : ('a -> unit) -> 'a array -> unit

val appi : (int * 'a -> unit) -> 'a array -> unit

val modify : ('a -> 'a) -> 'a array -> unit

val modifyi : (int * 'a -> 'a) -> 'a array -> unit
