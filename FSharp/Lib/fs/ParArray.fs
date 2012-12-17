module ParArray

(*shared*)
type 'a parArray = 'a array
(*readonly*)
type 'a readOnlyArray = 'a array

let array args = SML.Array.array args
let fromList args = SML.Array.fromList args
let fromArray (arr:'a array) = arr:'a parArray

let readOnly (arr:'a parArray) = arr:'a readOnlyArray

let length args = Array.length args
let sub args = SML.Array.sub args
let update args = SML.Array.update args

#if PARALLEL
let iteri = Array.Parallel.iteri
#else
let iteri = Array.iteri
#endif

let modify f arr =
    let iter i a = SML.Array.update (arr, i, f a)
    iteri iter arr

let modifyi f arr =
    let iter i a = SML.Array.update (arr, i, f (i,a))
    iteri iter arr

