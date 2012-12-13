module Point

open Constants

exception UnequalLengths

type t = double list

let fromList (list:double list):t = 
    if (List.length list) = NDIM then
        list
    else
        raise UnequalLengths

let fromArray (array:double array):t =
    if (Array.length array) = NDIM then
        SML.Array.foldr SML.List.cons [] array
    else
        raise UnequalLengths

let toList (p:t) = p

let toArray (p:t) = SML.Array.fromList (p:double list)

let constant (r:double) = SML.List.tabulate (NDIM,fun i -> r)

let zero = constant 0.0

let one = constant 1.0

let add (p1:t,p2:t) =
    let addOne (x:double,y:double) = x + y
    List.map addOne (SML.ListPair.zipEq (p1,p2))

let sub (p1:t,p2:t) =
    let minus (x,y) = x - y
    List.map minus (SML.ListPair.zipEq (p1,p2)) 

let adds (p:t,s:double) = List.map (fun x -> x + s) p

let muls (p:t,s:double) = List.map (fun x -> x * s) p

let divs (p:t,s:double) = List.map (fun x -> x / s) p

let dot (p1:t,p2:t) =
    let mul (x,y) = x * y
    let plus (x,y) = x + y
    let product = List.map mul (SML.ListPair.zipEq (p1,p2))
    SML.List.foldl plus 0.0 product

let toString (p:t) =
    let rec toString' list =
        match list with
            [] -> ""
          | x :: [] -> Util.realToString x
          | x :: tl ->
              (Util.realToString x) + "," + (toString' tl)
    "<" + (toString' p) + ">"

