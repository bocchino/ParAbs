structure BarnesHut =
struct

exception BadOpts

datatype optResult = SizeOpt of string
		   | TestOpt

open Body
open Constants
open DataGen

val nbody = ref 100000
val tree = Tree.new

(* Initialize the system *)
fun initSystem () =
    let
	(* Accumulated center of mass *)
	val cmr = ref Point.zero
        (* Accumulated velocity *)
	val cmv = ref Point.zero
	(* Empty body array *)
	val bodies:Body.body option array = Array.array (!nbody,NONE)
    in
	(Tree.setRmin tree (Point.const ~2.0);
	 Tree.setRsize tree (~2.0 * ~2.0);
	 Tree.setBodies tree bodies;

	 (* Create bodies and fill in array*)	
	 let
	     val i = ref 0
	 in
	     while !i < 32 do
		 (DataGen.uniformTestData (bodies,!i,cmr,cmv);
		  i := !i + 1)
	 end;

	 (* Normalize coordinates so average pos and vel are 0 *)
	 cmr := Point.divs (!cmr,Real.fromInt (!nbody));
	 cmv := Point.divs (!cmv,Real.fromInt (!nbody));
	 Array.modify (Util.opt (Body.normalize (cmr,cmv))) bodies;

	 (* Calculate the bounding box for the tree *)
	 Tree.calcBoundingBox tree)
    end

(* Advance n-body system one time step *)
fun stepSystem nstep = 
    let
	val rmin = Tree.getRmin tree
	val rsize = Tree.getRsize tree
	val bodies = Tree.getBodies tree
	fun indexFn {data:body,level:int} =
	    let
		val level' = Word32.toInt (Word32.>> (IMAX,Word.fromInt (level+1)))
	    in
		Tree.subindex (Tree.intcoord (data,rmin,rsize),level')
	    end
	val regionTree = RegionTree.empty NDIM indexFn
	val readOnlyRegionTree = RegionTree.readOnly regionTree
    in
	(Array.app (Util.optApp (RegionTree.insert regionTree)) bodies;
	 Tree.reorderBodies (tree,readOnlyRegionTree);
	 (* Fill in center-of-mass coordinates *)
	 ignore (RegionTree.reduce regionTree Tree.centerOfMass);
	 (* Compute gravity on particles *)
	 Gravity.computeForces (tree,readOnlyRegionTree,nstep);
	 (* Update particle positions *)
	 Gravity.updatePositions tree;
	 (* Print bodies, for testing *)
	 Body.printBodies (Tree.getBodies tree);
	 (* Recalculate the bounding box for the tree *)
	 Tree.calcBoundingBox tree)
    end	

(* Do the simulation *)
fun doSimulation () =
    let
	val tnow = ref 0.0
	val tout = ref 0.0
	val i = ref 0
    in
	while (!tnow < Constants.tstop + 0.1 * Constants.dtime)
	      andalso (!i < NSTEPS) do
	    (stepSystem (!i);
	     tnow := !tnow + Constants.dtime;
	     i := !i + 1)
    end

val options = [{short="s",
		long=["size"],
		desc=GetOpt.ReqArg (SizeOpt,"size"),
		help="set data size"},
	       {short="t",
		long=["test"],
		desc=GetOpt.NoArg (fn () => TestOpt),
		help="generate test output"}]

fun processOpt (SizeOpt str) = (case Int.fromString str of
				    SOME n => nbody := n
				  | NONE => raise BadOpts)
  | processOpt TestOpt = Util.testMode := true

fun main (name,args) =
    (case GetOpt.getOpt {argOrder=GetOpt.Permute,
			  options=options,
			  errFn = fn s => print s}
			 args of
	  (opts,[]) => (List.app processOpt opts;
			if !nbody mod 32 = 0 then
			    (initSystem ();
			     doSimulation();
			     OS.Process.success)
			else
			    Util.err "data size must be divisible by 32")
	|_ => raise BadOpts)
    handle BadOpts => (TextIO.output (TextIO.stdErr,GetOpt.usageInfo
							{header="usage: barnes-hut [opts]",
							 options=options}); 
		       OS.Process.failure)
	 | e => Util.err "exception occurred"
end
