module RegionTree

type 'a node = Inner of 'a inner
             | Leaf of 'a ref
and 'a inner = {data:'a option ref;
                children:'a node option array}

type 'a indexFn = 'a * int -> int

type 'a tree = {arity:int;
                root:'a node option ref;
                indexFn:'a indexFn}

type 'a readOnlyTree = 'a tree
type 'a readOnlyNode = 'a node

type 'a reduction = 'a option -> 'a option list -> 'a option

let empty ndim indexFn = 
    {arity=1 <<< ndim;
     root=ref None;
     indexFn=indexFn}

let insert {arity=arity;root=root;indexFn=indexFn} newData =
    let rec insert' root newData level =
        Some (match root with
              None -> Leaf (ref newData)
            | Some (Leaf oldData) ->
              let children = SML.Array.array (arity, None)
              let inner = {data=ref None;children=children}
              let idx = indexFn (!oldData,level)
              (SML.Array.update (children,idx,root);
               insert'' inner newData level)
            | Some (Inner inner) -> insert'' inner newData level)
    and insert'' {data=data;children=children} newData level =
        let idx = indexFn (newData,level)
        let child = SML.Array.sub (children,idx)
        let child = insert' child newData (level+1)
        (SML.Array.update (children,idx,child);
         Inner {data=data;children=children})
    root := insert' (!root) newData 0

let reduce {arity=arity;root=root;indexFn=indexFn} reduction =
    let rec reduce' root =
        match root with
            Some (Leaf data) -> 
            let result = match reduction (Some (!data)) [] with
                             Some data' -> data'
                           | None       -> !data
            (data := result; Some result)
          | Some (Inner {data=data;children=children}) -> 
            let childList = SML.Array.foldl SML.List.cons [] children
            (* TODO: Should be parallel *)
            let dataList = List.map reduce' childList
            let result = reduction (!data) dataList
            (data := result; result)
          | None -> None
    reduce' (!root)
    
let readOnly<'a> (tree:'a tree):'a readOnlyTree = tree

let getRoot {arity=arity;root=root;indexFn=indexFn} = !root

let getData node =
    match node with
        Some (Leaf data) -> Some (!data)
      | Some (Inner {data=data;children=_}) -> !data
      | None -> None

let isLeaf node =
    match node with
        Leaf _ -> true
      | _      -> false

let getChildren node =
    match node with
        Leaf _ -> None
      | Inner {data=_;children=children} -> Some children


