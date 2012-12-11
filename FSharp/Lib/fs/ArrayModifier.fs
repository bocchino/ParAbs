module ArrayModifier

(*shared*)
type modifier<'a, (*readonly*) 'b> = 'a array * 'b

type modifyFn<'a, (*readonly*) 'b> = 'b -> 'a -> 'a

type modifyiFn<'a, (*readonly*) 'b> = 'b -> (int * 'a) -> 'a

let modifier (sz,init) bInit =
    (Array.create sz init,bInit)

let modify (array, readOnlyState) modifyFn =
    let modifyFn' = modifyFn readOnlyState
    let iter i a = Array.set array i (modifyFn' a)
    Array.Parallel.iteri iter array

let modifyi (array,readOnlyState) modifyiFn =
    let modifyiFn' = modifyiFn readOnlyState
    let iter i a = Array.set array i (modifyiFn' (i,a))
    Array.Parallel.iteri iter array

let getArray ((array,_):modifier<'a,'b>) = array

