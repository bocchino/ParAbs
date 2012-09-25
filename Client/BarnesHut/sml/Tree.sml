structure Tree =
struct

open Array
open Body
open Constants

type t = {rmin:Point.t ref,
	  rsize:real ref,
	  bodies:body option array ref}

val new:t = {rmin=ref Point.zero,
	     rsize=ref 0.0,
	     bodies=ref (Array.array (0,NONE))}

fun setRmin {rmin,rsize,bodies} p = rmin := p
fun setBodies {rmin,rsize,bodies} a = bodies := a
fun setRsize {rmin,rsize,bodies} r = rsize := r

fun getRmin {rmin,rsize,bodies} = !rmin
fun getRsize {rmin,rsize,bodies} = !rsize
fun getBodies {rmin,rsize,bodies} = !bodies

fun calcBoundingBox {rmin,rsize,bodies} =
    let
	val max = Point.toArray (Point.const Real.minPos)
	val min = Point.toArray (Point.const Real.maxFinite)
	fun calcOneDim (i,x) = (if x < Array.sub (min,i) then
				    Array.update (min,i,x)
				else ();
				if x > Array.sub (max,i) then
				    Array.update (max,i,x)
				else ())
	fun calcAllDims (body:body option) =
	    case body of
		SOME body => Array.appi calcOneDim 
					(Point.toArray (Body.getPos body))
	      | NONE => ()
    in
	(Array.app calcAllDims (!bodies);
	 let
	     val min = Point.fromArray min
	     val max = Point.sub(Point.fromArray max,min)
	     val side = ref 0.0
	     fun calcSide x = if !side < x then
				  side := x
			      else ()
	 in
	     (List.app calcSide (Point.toList max);
	      rmin := Point.adds(min,~(!side)/100000.0);
	      rsize := 1.00002 * (!side))
	 end)
    end

(* Find the sub index into the cell children *)
fun subindex (coords:int vector,level:int) =
    let
	val yes = ref false
	val i = ref 0
	val testWord = Word32.fromInt level
	fun testCoord x =
	    let
		val cWord = Word32.fromInt (Vector.sub (coords,x))
	    in
		not ((Word32.andb (cWord,testWord)) = Word32.fromInt 0)
	    end
    in
	(if testCoord 0 then
	     (i := !i + (Util.>> (NSUB,1));
	      yes := true)
	 else ();
	 let
	     val k = ref 1
	 in
	     while !k < NDIM do
		 (if ((testCoord (!k)) andalso not (!yes)) orelse 
		     ((not (testCoord (!k))) andalso !yes) then
		      (i := !i + Util.>> (NSUB,!k+1);
		       yes := true)
		  else
		      yes := false;
		  k := !k + 1)
	 end;
	 !i)
    end

(* Compute integerized coordinates *)
fun intcoord (body:body,rmin:Point.t,rsize:real) =
    let
	val pos = Point.toList (Body.getPos body)
	val rmin = Point.toList rmin
	fun oneDim (pos_i,rmin_i) =
	    let
		val xsc = (pos_i - rmin_i) / rsize
	    in
		if 0.0 <= xsc andalso xsc < 1.0 then
		    Real.floor (IMAX_REAL * xsc)
		else
		    0
	    end
    in
	Vector.fromList (ListPair.mapEq oneDim (pos,rmin))
    end

(* Reorder the body array to capture the positioning in the tree *)
fun reorderBodies (tree,readOnlyRegionTree) =
    let
	val bodies = getBodies tree
	val newBodies = Array.array (Array.length bodies,NONE:body option)
	val index = ref 0
	fun reorderBodies' nodeOpt =
	    case nodeOpt of
		SOME node =>
		(case RegionTree.getChildren node of
		     SOME children => Array.app reorderBodies' children
		   | NONE => (Array.update (newBodies,!index,RegionTree.getData node);
			      index := !index + 1))
	      | NONE => ()
    in
	(reorderBodies' (RegionTree.getRoot readOnlyRegionTree);
	 setBodies tree newBodies)
    end

(* Reduction function for center-of-mass computation *)
fun centerOfMass (bodyOpt:body option) (bodiesOpt:body option list) =
    case bodiesOpt of
	[] => bodyOpt
      | _  => let
	    fun processChild NONE = NONE
	      | processChild (SOME body) =
		let
		    val pos = Body.getPos body
		    val mass = Body.getMass body
		in
		    SOME (mass,Point.muls (pos,mass))
		end
	    fun combine (SOME body,(mass,pos)) =
		((Body.getMass body) + mass,
		 Point.add ((Body.getPos body),pos))
	      | combine (NONE,(mass,pos)) = (mass,pos)
	    val (mass,pos) = List.foldl combine (0.0,Point.zero) bodiesOpt 
	    val normalized = (mass,Point.divs (pos,mass))
	in
	    SOME (Body.updateMassPos bodyOpt normalized)
	end

(* Advance n-body system one time step *)
fun stepSystem (tree,nstep) = 
    let
	val rmin = getRmin tree
	val rsize = getRsize tree
	val bodies = getBodies tree
	fun indexFn {data:body,level:int} =
	    let
		val level' = Word32.toInt (Word32.>> (IMAX,Word.fromInt (level+1)))
	    in
		subindex (intcoord (data,rmin,rsize),level')
	    end
	val regionTree = RegionTree.empty NDIM indexFn
	val readOnlyRegionTree = RegionTree.readOnly regionTree
    in
	(Array.app (Util.optApp (RegionTree.insert regionTree)) bodies;
	 reorderBodies (tree,readOnlyRegionTree);
	 printBodies (getBodies tree);
	 (* Fill in center-of-mass coordinates *)
	 ignore (RegionTree.reduce regionTree centerOfMass);
	 (* Stop after one time step, for now *)
	 OS.Process.exit OS.Process.success)
    end	

end
