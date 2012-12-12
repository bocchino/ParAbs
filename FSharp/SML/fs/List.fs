module SML.List

let cons x y = x :: y

let foldl f init arr =
    let f' = fun x -> fun y -> f y x
    in List.fold f' init arr
    
let foldr f init arr =
    List.foldBack f arr init
