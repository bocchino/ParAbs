structure Tree =
struct

open Array
open Body
open Constants

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

fun calcBoundingBox {rmin,rsize,bodies} =
    let
	val max = Point.toArray (Point.const Real.minPos)
	val min = Point.toArray (Point.const Real.maxFinite)
	fun calcOneDim (i,x) = (if x < Array.sub (min,i) then
				    Array.update (min,i,x)
				else ();
				if x > Array.sub (max,i) then
				    Array.update (max,i,x)
				else ())
	fun calcAllDims (body:body option) =
	    case body of
		SOME body => Array.appi calcOneDim 
					(Point.toArray (Body.getPos body))
	      | NONE => ()
    in
	(Array.app calcAllDims (!bodies);
	 let
	     val min = Point.fromArray min
	     val max = Point.sub(Point.fromArray max,min)
	     val side = ref 0.0
	     fun calcSide x = if !side < x then
				  side := x
			      else ()
	 in
	     (List.app calcSide (Point.toList max);
	      rmin := Point.adds(min,~(!side)/100000.0);
	      rsize := 1.00002 * (!side))
	 end)
    end

fun stepSystem tree nstep = 
    (* TODO *)
    print "step system!\n"

end
