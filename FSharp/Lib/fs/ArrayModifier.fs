module ArrayModifier

(*shared*)
type modifier<'a, (*readonly*) 'b> = array<'a> * 'b

type modifyFn<'a, (*readonly*) 'b> = 'b -> 'a -> 'a

type modifyiFn<'a, (*readonly*) 'b> = 'b -> (int * 'a) -> 'a

let modifier spec bInit =
    (SML.Array.array spec, bInit)

let iteri = Array.iteri (* parallel *)

let modify (array, readOnlyState) modifyFn =
    let modifyFn' = modifyFn readOnlyState
    let iter i a = SML.Array.update (array, i, modifyFn' a)
    iteri iter array

let modifyi (array,readOnlyState) modifyiFn =
    let modifyiFn' = modifyiFn readOnlyState
    let iter i a = SML.Array.update (array, i, modifyiFn' (i,a))
    iteri iter array

let getArray ((array,_):modifier<'a,'b>) = array

