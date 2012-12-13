module Tree

open Body
open Constants

type t = {rmin:Point.t ref;
          rsize:float ref;
          bodies:body option array ref}

let create:t = {rmin=ref Point.zero;
                rsize=ref 0.0;
                bodies=ref (SML.Array.array (0,None))}

let setRmin {rmin=rmin;rsize=rsize;bodies=bodies} p = rmin := p
let setBodies {rmin=rmin;rsize=rsize;bodies=bodies} a = bodies := a
let setRsize {rmin=rmin;rsize=rsize;bodies=bodies} r = rsize := r

let getRmin {rmin=rmin;rsize=rsize;bodies=bodies} = !rmin
let getRsize {rmin=rmin;rsize=rsize;bodies=bodies} = !rsize
let getBodies {rmin=rmin;rsize=rsize;bodies=bodies} = !bodies

(* Calculate bounding box for all bodies *)
let calcBoundingBox {rmin=rmin;rsize=rsize;bodies=bodies} =
    let max = Point.toArray (Point.constant System.Double.Epsilon)
    let min = Point.toArray (Point.constant System.Double.MaxValue)
    let calcOneDim (i,x) = (if x < SML.Array.sub (min,i) then
                                SML.Array.update (min,i,x)
                            else ();
                            if x > SML.Array.sub (max,i) then
                                SML.Array.update (max,i,x)
                            else ())
    let calcAllDims (body:body option) =
        match body with
            Some body ->
                SML.Array.appi calcOneDim (Point.toArray (Body.getPos body))
          | None -> ()
    (SML.Array.app calcAllDims (!bodies);
     let min = Point.fromArray min
     let max = Point.sub (Point.fromArray max,min)
     let side = ref 0.0
     let calcSide x = if !side < x then
                          side := x
                      else ()
     (SML.List.app calcSide (Point.toList max);
      rmin := Point.adds(min,-(!side)/100000.0);
      rsize := 1.00002 * (!side)))

(* Find the sub index into the cell children *)
let subindex (coords:int array,level:int) =
    let yes = ref false
    let i = ref 0
    let testWord = level
    let testCoord x =
        let cWord = SML.Array.sub (coords,x)
        not ((cWord &&& testWord) = 0)
    (if testCoord 0 then
         (i := !i + (NSUB >>> 1);
          yes := true)
     else ();
     let k = ref 1
     while !k < NDIM do
         (if ((testCoord (!k)) && not (!yes)) || 
             ((not (testCoord (!k))) && !yes) then
              (i := !i + (NSUB >>> (!k+1));
               yes := true)
          else
              yes := false;
          k := !k + 1);
     !i)

(* Compute integerized coordinates for a body *)
let intcoord (body:body,rmin:Point.t,rsize:float) =
    let pos = Point.toList (Body.getPos body)
    let rmin = Point.toList rmin
    let oneDim (pos_i,rmin_i) =
        let xsc = (pos_i - rmin_i) / rsize
        if 0.0 <= xsc && xsc < 1.0 then
            int (System.Math.Floor (IMAX_REAL * xsc))
        else
            0
    SML.Array.fromList (SML.ListPair.mapEq oneDim (pos,rmin))

(* Reorder the body array to capture the positioning in the tree *)
let reorderBodies (tree,readOnlyRegionTree) =
    let bodies = getBodies tree
    let newBodies = SML.Array.array (Array.length bodies,None)
    let index = ref 0
    let rec reorderBodies' nodeOpt =
        match nodeOpt with
            Some node ->
            (match RegionTree.getChildren node with
                 Some children -> SML.Array.app reorderBodies' children
               | None -> (SML.Array.update (newBodies,!index,RegionTree.getData (Some node));
                          index := !index + 1))
          | None -> ()
    (reorderBodies' (RegionTree.getRoot readOnlyRegionTree);
     setBodies tree newBodies)

(* Reduction function for center-of-mass computation *)
let centerOfMass (bodyOpt:body option) (bodiesOpt:body option list) =
    match bodiesOpt with
        [] -> bodyOpt
      | _  -> 
            let combine (arg:body option * (double * Point.t)):double * Point.t =
                match arg with
                    (Some body,(mass,pos)) ->
                        let bodyPos = Body.getPos body
                        let bodyMass = Body.getMass body
                        let bodyMoment = Point.muls (bodyPos,bodyMass)
                        (bodyMass + mass,
                         Point.add (pos,bodyMoment))
                  | (None,mp) -> mp
            let (mass,pos) = SML.List.foldl combine (0.0,Point.zero) bodiesOpt 
            let normalized = (mass,Point.divs (pos,mass))
            Body.updateMassPosOpt bodyOpt normalized
