module BarnesHut

exception BadOpts

type optResult = SizeOpt of string
               | TestOpt

open Body
open Constants
open DataGen


let nbody = ref 64
let tree = Tree.create

(* Initialize the system *)
let initSystem () =
    (* Accumulated center of mass *)
    let cmr = ref Point.zero
    (* Accumulated velocity *)
    let cmv = ref Point.zero
    (* Empty body array *)
    let bodies:Body.body option array = SML.Array.array (!nbody,None)
    (Tree.setRmin tree (Point.constant -2.0);
     Tree.setRsize tree (-2.0 * -2.0);
     Tree.setBodies tree bodies;

     (* Create bodies and fill in array*)   
     let i = ref 0
     while !i < 32 do
         (DataGen.uniformTestData (bodies,!i,cmr,cmv);
         i := !i + 1);

     (* Normalize coordinates so average pos and vel are 0 *)
     cmr := Point.divs (!cmr,float (!nbody));
     cmv := Point.divs (!cmv,float (!nbody));
     SML.Array.modify (Util.opt (Body.normalize (cmr,cmv))) bodies)

(* Advance n-body system one time step *)
let stepSystem nstep = 
   (* Calculate the bounding box for the tree *)
   let _ = Tree.calcBoundingBox tree
   let rmin = Tree.getRmin tree
   let rsize = Tree.getRsize tree
   let bodies = Tree.getBodies tree
   let indexFn (data:body,level:int) =
       let level' = IMAX >>> (level+1)
       Tree.subindex (Tree.intcoord (data,rmin,rsize),level')
   let regionTree = RegionTree.empty NDIM indexFn
   let readOnlyRegionTree = RegionTree.readOnly regionTree
   (SML.Array.app (Util.optApp (RegionTree.insert regionTree)) bodies;
    Tree.reorderBodies (tree,readOnlyRegionTree);
    (* Fill in center-with-mass coordinates *)
    ignore (RegionTree.reduce regionTree Tree.centerOfMass);
    (* Compute gravity on particles *)
    Gravity.computeForces (tree,readOnlyRegionTree,nstep);
    (* Update particle positions *)
    Gravity.updatePositions tree)

(* Do the simulation *)
let doSimulation () =
    let tnow = ref 0.0
    let tout = ref 0.0
    let i = ref 0
    while (!tnow < Constants.tstop + 0.1 * Constants.dtime)
      && (!i < NSTEPS) do
        (stepSystem (!i);
         tnow := !tnow + Constants.dtime;
         i := !i + 1)

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some (s.Substring (p.Length))
    else
        None

let usage () =
    printfn "usage: barnes-hut [opts]"
    printfn "  --size=size  set data size"
    printfn "  --output     generate test output"

let parseArg i arg =
    try
        match (i,arg) with
          | (0,_)                -> ()
          | (_,Prefix "--output" "") -> Util.outputMode := true
          | (_,Prefix "--size=" sz)  -> nbody := System.Int32.Parse sz
          | _                        -> raise BadOpts
    with
        _ -> usage ()

let args = System.Environment.GetCommandLineArgs()
Array.iteri parseArg args

if !nbody % 32 = 0 then
    (initSystem ();
     doSimulation();
     (* Print bodies, for testing *)
     Body.printBodies (Tree.getBodies tree))
else
    Util.err "data size must be divisible by 32"


