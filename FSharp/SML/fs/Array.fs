module SML.Array

let array (len,init) =
    [| for i in 1 .. len -> init |]    

let sub (arr,idx) =
    Array.get arr idx

let update (arr,idx,elt) =
    Array.set arr idx elt

let foldl f init arr =
    let f' = fun x -> fun y -> f (y,x)
    in Array.fold f' init arr
    
let foldr f init arr =
    let f' = fun x -> fun y -> f (x,y)
    Array.foldBack f' arr init

let fromList (list:'a list) = [| for elt in list -> elt |]

let app f arr = Array.iter f arr

let appi f arr =
    let f' = fun x -> fun y -> f (x,y)
    Array.iteri f' arr
