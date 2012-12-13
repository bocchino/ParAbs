module SML.List

let cons x y = x :: y

let foldl f init arr =
    let f' = fun x -> fun y -> f (y,x)
    in List.fold f' init arr
    
let foldr f init arr =
    let f' = fun x -> fun y -> f (x,y)
    List.foldBack f' arr init

let tabulate (n,f) =
    let rec tabulate' (i,result) =
        if i < 0 then
            result
        else
            tabulate' (i - 1, f(i) :: result)
    tabulate' (n - 1,[])
    
let app f arr = List.iter f arr

let appi f arr =
    let f' = fun x -> fun y -> f (x,y)
    List.iteri f' arr
