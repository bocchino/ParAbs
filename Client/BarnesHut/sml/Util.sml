structure Util =
struct

val A = 16807.0
val M = 2147483647.0

fun rand (seed:real) =
    let
	val t = A * seed + 1.0
    in
	t - (M * (Real.realFloor (t / M)))
    end

fun xrand (xl,xh,r) =
    xl + (xh - xl) * r / 2147483647.0

val logging = ref false

fun log s = print s

end
