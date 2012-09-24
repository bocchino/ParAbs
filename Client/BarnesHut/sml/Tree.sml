structure Tree =
struct

open Array
open Body

type t = {rmin:Point.t ref,
	  rsize:real ref,
	  bodies:body option array ref}

val new:t = {rmin=ref Point.zero,
	     rsize=ref 0.0,
	     bodies=ref (Array.array (0,NONE))}

fun setRmin {rmin,rsize,bodies} p = rmin := p
fun setBodies {rmin,rsize,bodies} a = bodies := a
fun setRsize {rmin,rsize,bodies} r = rsize := r

fun getBodies {rmin,rsize,bodies} = !bodies

end
