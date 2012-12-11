structure ArrayModifier :> ARRAY_MODIFIER =
struct

(*shared*) type ('a, (*readonly*) 'b) modifier = 'a Array.array * 'b

fun modifier aInit bInit =
    (Array.array aInit,bInit)

type ('a, (*readonly*) 'b) modifyFn = 'b -> 'a -> 'a
type ('a, (*readonly*) 'b) modifyiFn = 'b -> (int * 'a) -> 'a

fun modify (array,readOnlyState) modifyFn =
    (* TODO: Should be parallel *)
    Array.modify (modifyFn readOnlyState) array

fun modifyi (array,readOnlyState) modifyiFn =
    (* TODO: Should be parallel *)
    Array.modifyi (modifyiFn readOnlyState) array

fun getArray (array,_) = array

end
