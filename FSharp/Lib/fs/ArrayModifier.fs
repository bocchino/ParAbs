module ArrayModifier

(*shared*)
type modifier<'a, (*readonly*) 'b> = array<'a> * 'b

type modifyFn<'a, (*readonly*) 'b> = 'b -> 'a -> 'a

type modifyiFn<'a, (*readonly*) 'b> = 'b -> (int * 'a) -> 'a

let modifier spec bInit =
    (SML.Array.array spec, bInit)

let modify (array, readOnlyState) modifyFn =
    let modifyFn' = modifyFn readOnlyState
    let iter i a = SML.Array.update (array, i, modifyFn' a)
    Array.Parallel.iteri iter array

let modifyi (array,readOnlyState) modifyiFn =
    let modifyiFn' = modifyiFn readOnlyState
    let iter i a = SML.Array.update (array, i, modifyiFn' (i,a))
    Array.Parallel.iteri iter array

let getArray ((array,_):modifier<'a,'b>) = array

