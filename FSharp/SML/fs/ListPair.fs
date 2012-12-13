module SML.ListPair

let zipEq (x,y) =
    List.zip x y

let mapEq f (l1,l2) =
    let pairs = zipEq(l1,l2)
    List.map f pairs
