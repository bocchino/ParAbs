module SML.List

val cons : 'a -> 'a list -> 'a list

val foldl : ('a *'b -> 'b) -> 'b -> 'a list -> 'b

val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b

val tabulate : int * (int -> 'a) -> 'a list

val app : ('a -> unit) -> 'a list -> unit

val appi : (int * 'a -> unit) -> 'a list -> unit

