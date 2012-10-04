structure ParArray :> PAR_ARRAY =
struct

(*shared*)   type 'a array = 'a Array.array
(*readonly*) type 'a readOnlyArray = 'a array

fun array args = Array.array args
fun fromList args = Array.fromList args
fun fromArray arr = arr

fun readOnly arr = arr

fun length args = Array.length args
fun sub args = Array.sub args
fun update args = Array.update args

(* TODO: Should be parallel *)
fun modifyi args = Array.modifyi args
fun modify args = Array.modify args


end
