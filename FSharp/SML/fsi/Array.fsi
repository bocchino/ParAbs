module SML.Array

val array : int * 'a -> 'a array

val sub : 'a array * int -> 'a

val update : 'a array * int * 'a  -> unit

val foldl : ('a -> 'b -> 'b) -> 'b -> 'a array -> 'b

val foldr : ('a -> 'b -> 'b) -> 'b -> 'a array -> 'b

val fromList : 'a list -> 'a array

