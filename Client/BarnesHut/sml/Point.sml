structure Point :> POINT =
struct

open Constants

type t = real list

fun fromList list = 
    if (List.length list) = NDIM then
	list
    else
	raise ListPair.UnequalLengths

fun const (r:real) = List.tabulate (NDIM,fn i => r)

val zero = const 0.0

val one = const 1.0

fun add (p1:t,p2:t) = 
    List.map op+ (ListPair.zipEq (p1,p2))

fun sub (p1:t,p2:t) = 
    List.map op- (ListPair.zipEq (p1,p2)) 

fun adds (p:t,s:real) = List.map (fn x => x + s) p

fun muls (p:t,s:real) = List.map (fn x => x * s) p

fun divs (p:t,s:real) = List.map (fn x => x / s) p

fun dot (p1:t,p2:t) = 
    List.foldl op+ 0.0 
	       (List.map op* (ListPair.zipEq (p1,p2)))

fun toString (p:t) =
    let 
	fun toString' [] = ""
	  | toString' (x :: []) = Real.toString x
	  | toString' (x :: tl) =
	    (Real.toString x) ^ "," ^ (toString' tl)
    in
	"<" ^ (toString' p) ^ ">"
    end
	    
end

