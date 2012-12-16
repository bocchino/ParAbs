module Gravity

open Body
open Constants

type gravityData =
  struct
    val skipID:int;
    val pos0:Point.t;
    val mutable phi0:float
    val mutable acc0:Point.t
    val mutable ai:Point.t
    val mutable dr:Point.t
    new (a:int, b:Point.t, c:float , d:Point.t , e:Point.t , f:Point.t ) =
      { skipID = a; pos0 = b; phi0 = c; acc0 = d; ai = e; dr = f; }
    member this.setphi0 (x) = this.phi0 <- x
    member this.setdr (x) = this.dr <- x
    member this.setacc0 (x) = this.acc0 <- x
    member this.setai (x) = this.ai <- x
  end

let getSkipID (a : gravityData) = a.skipID;
let getPos0   (a : gravityData) = a.pos0
let getPhi0   (a : gravityData) = a.phi0
let getAcc0   (a : gravityData) = a.acc0
let getAI     (a : gravityData) = a.ai
let getDR     (a : gravityData) = a.dr

(* Should we subdivide a node? *)
let subdivide (node:body RegionTree.readOnlyNode)
              (dsq:float,tolsq:float,gd:gravityData) =
    if RegionTree.isLeaf node then
        false
    else
        match RegionTree.getData (Some node) with
            None -> false
          | Some body ->
              let pos = Body.getPos body
              let pos0 = getPos0 gd
              let dr = Point.sub (pos,pos0)
              let drsq = Point.dot (dr,dr)
              gd.setdr dr
              tolsq * drsq < dsq

(* Compute single body-body interaction *)
let twoBodies (body:body,gd:gravityData) =
    if Body.getID body = getSkipID gd then
        (* Don't interact body with itself *)
        ()
    else
        let acc0 = getAcc0 gd
        let dr = Point.sub (Body.getPos body,getPos0 gd)
        let drsq = Point.dot (dr,dr) + eps * eps
        let drabs = sqrt drsq
        let phi0 = getPhi0 gd
        let phii = (Body.getMass body) / drabs
        let mor3 = phii / drsq
        let ai = Point.muls (dr,mor3)
        gd.setphi0 (phi0 - phii)
        gd.setai ai
        gd.setacc0 (Point.add (acc0,ai))
        gd.setdr dr
                   
(* Recursively compute gravity induced by tree *)
let bodyTree (tree:body RegionTree.readOnlyTree,
              rsize:float,gd:gravityData) =
    let tolsq = tol*tol
    let rec computeGravity' (node:body RegionTree.readOnlyNode) (dsq:float) =
        if subdivide node (dsq,tolsq,gd) then
            let processChild node =
                computeGravity' node (dsq / 4.0)
            match RegionTree.getChildren node with
                None -> ()
              | Some children -> SML.Array.app (Util.optApp processChild) children
        else
            match RegionTree.getData (Some node) with
                Some body -> twoBodies (body,gd)
              | None -> ()
    match RegionTree.getRoot tree with
        Some node -> computeGravity' node (rsize * rsize)
      | None -> ()

(* Compute interaction for all bodies *)
let allBodies (tree:body RegionTree.readOnlyTree,
               rsize:float,
               bodies:body option array,
               nstep:int) =
    let modifyiFn (tree,bodies) (i,_) =
        match ParArray.sub (bodies,i) with
            None      -> None
          | Some body ->
              let dthf = 0.5 * dtime
              let acc1 = Body.getAcc body
              let gd = new gravityData ( Body.getID body,
                         Body.getPos body,
                          0.0,
                          Point.zero,
                          Point.zero,
                          Point.zero)
              bodyTree (tree,rsize,gd)
              let acc0 = getAcc0 gd
              let newBody =
                  if nstep > 0 then
                      let phi0 = getPhi0 gd
                      let dacc = Point.sub (acc0,acc1)
                      let dvel = Point.muls (dacc,dthf)
                      let vel = Point.add (Body.getVel body,dvel)
                      Body.updateVelAccPhi body (vel,acc0,phi0)
                  else
                      Body.updateAcc body acc0
              Some newBody
    let readOnlyBodies = ParArray.readOnly (ParArray.fromArray bodies)
    let modifier =
        ArrayModifier.modifier (Array.length bodies,None) (tree,readOnlyBodies)
    let newBodies = ArrayModifier.getArray modifier
    ArrayModifier.modifyi modifier modifyiFn
    newBodies

(* Compute gravity on particles *)
let computeForces (tree:Tree.t,
                   regionTree:body RegionTree.readOnlyTree,
                   nstep:int) =
    let bodies = Tree.getBodies tree
    let rsize = Tree.getRsize tree
    let newBodies = allBodies (regionTree,
                               rsize,bodies,nstep)
    Tree.setBodies tree newBodies

(* Update particle positions *)
let updatePositions (tree:Tree.t) =
    let bodies = Tree.getBodies tree
    let oneBody body:body =
        let acc = Body.getAcc body
        let vel = Body.getVel body
        let pos = Body.getPos body
        let dthf = 0.5 * dtime
        let dvel = Point.muls (acc,dthf)
        let vel1 = Point.add (vel,dvel)
        let dpos = Point.muls (vel1,dtime)
        let pos = Point.add (pos,dpos)
        let vel = Point.add(vel1,dvel)
        Body.updatePosVel body (pos,vel)
    SML.Array.modify (Util.opt oneBody) bodies


