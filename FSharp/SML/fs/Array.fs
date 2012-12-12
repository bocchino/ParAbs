module SML.Array

let array (len,init) =
    [| for i in 1 .. len -> init |]    

let sub (arr,idx) =
    Array.get arr idx

let update (arr,idx,elt) =
    Array.set arr idx elt

let foldl f init arr =
    let f' = fun x -> fun y -> f y x
    in Array.fold f' init arr
    
let foldr f init arr =
    Array.foldBack f arr init

    
