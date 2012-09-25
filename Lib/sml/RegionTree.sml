structure RegionTree :> REGION_TREE =
struct

datatype 'a node = Inner of {data:'a option ref,
			     children:'a node option array}
		 | Leaf of 'a ref

type 'a indexFn = {data:'a,level:int} -> int

type 'a tree = {arity:int,
		root:'a node option ref,
		indexFn:'a indexFn}

type 'a readOnlyTree = 'a tree
type 'a readOnlyNode = 'a node

type 'a reduction = 'a option -> 'a option list -> 'a option

fun empty ndim indexFn = 
    {arity=Word.toInt (Word.<< (Word.fromInt 1, Word.fromInt ndim)),
     root=ref NONE,
     indexFn=indexFn}

fun insert {arity,root,indexFn} newData =
    let
	fun insert' root newData level =
	    SOME (case root of
		      NONE => Leaf (ref newData)
		    | SOME (Leaf (ref oldData)) =>
		      let
			  val children = Array.array (arity,NONE)
			  val inner = {data=ref NONE,children=children}
			  val idx = indexFn {data=oldData,level=level}
		      in
			  (Array.update (children,idx,root);
			   insert'' inner newData level)
		      end
		    | SOME (Inner inner) => insert'' inner newData level)
	and insert'' {data,children} newData level =
	    let
		val idx = indexFn {data=newData,level=level}
	    in
		(Array.update (children,idx,insert' (Array.sub (children,idx)) 
						    newData (level+1));
		 Inner {data=data,children=children})
	    end
    in
	root := insert' (!root) newData 0
    end

fun reduce {arity,root,indexFn} reduction =
    let
	fun reduce' root =
	    case root of
		SOME (Leaf data) => 
		let
		    val result = case reduction (SOME (!data)) [] of
				     SOME data' => data'
				   | NONE       => !data
		in 
		    (data := result; SOME result)
		end
	      | SOME (Inner {data,children}) => 
		let
		    val childList = Array.foldl (op::) [] children
		    (* TODO: Should be parallel *)
		    val dataList = List.map reduce' childList
		    val result = reduction (!data) dataList
		in
		    (data := result; result)
		end
	      | NONE => NONE
    in
	reduce' (!root)
    end
    
fun readOnly tree = tree

fun getRoot {arity,root,indexFn} = !root

fun getData node =
    case node of
	Leaf data => SOME (!data)
      | Inner {data,children} => !data

fun isLeaf node =
    case node of
	Leaf _ => true
      | _      => false

fun getChildren node =
    case node of
	Leaf _ => NONE
      | Inner {data,children} => SOME children

end
