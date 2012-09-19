structure ParTree =
struct

datatype 'a node = Inner of {data:'a option,
				  children:('a node option) array}
		      | Leaf of 'a

type 'a indexFn = {data:'a,level:int} -> int

type 'a tree = {arity:int,
		root:'a node option,
		indexFn:'a indexFn} ref

fun empty arity indexFn = 
    ref {arity=arity,
	 root=NONE,
	 indexFn=indexFn}

fun insert tree newData =
    let
	val {arity,root,indexFn} = !tree
	fun loadtree root newData level =
	    case root of
		NONE => SOME (Leaf newData)
	      | SOME (Leaf oldData) =>
		let
		    val children = Array.array (arity,NONE)
		    val inner = {data=NONE,children=children}
		    val idx = indexFn {data=oldData,level=level}
		in
		    Array.update (children,idx,root);
		    SOME (loadtree' inner newData level)
		end
	      | SOME (Inner inner) => SOME (loadtree' inner newData level)
	and loadtree' {data,children} newData level =
	    let
		val idx = indexFn {data=newData,level=level}
	    in
		(Array.update (children,idx,loadtree (Array.sub (children,idx)) 
						     newData (level+1));
		 Inner {data=data,children=children})
	    end
    in
	tree := {arity=arity,
		 root=loadtree root newData 0,
		 indexFn=indexFn}
    end

end
