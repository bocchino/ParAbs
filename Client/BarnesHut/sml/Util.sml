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

fun opt f =
    fn x => case x of SOME s => SOME (f s)
		    | NONE => NONE

fun optApp f =
    fn x => case x of SOME s => f s
		    | NONE => ()

fun printOpt (SOME s) = print s
  | printOpt NONE = print "NONE"

fun printRet () = print "\n"

fun realToString r =
    let
	fun trans #"~" = "-"
	  | trans c = Char.toString c
	val substr = Substring.full (Real.fmt (StringCvt.FIX (SOME 6)) r)
    in
	Substring.translate trans substr
    end

fun err str =
    (TextIO.output (TextIO.stdErr,"barnes-hut: " ^ str ^ "\n");
     OS.Process.failure)

fun >> (i:int,shamt:int) =
    Word32.toInt (Word32.>> (Word32.fromInt i,Word.fromInt shamt))

end
