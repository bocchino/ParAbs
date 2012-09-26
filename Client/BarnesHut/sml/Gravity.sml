structure Gravity =
struct

open Body
open Constants

type gravityData = {skipID:int,
		    pos0:Point.t,
		    phi0:real ref,
		    acc0:Point.t ref,
		    ai:Point.t ref,
		    dr:Point.t ref}

fun getSkipID ({skipID,...}:gravityData) = skipID
fun getPos0 ({pos0,...}:gravityData) = pos0
fun getPhi0 ({phi0,...}:gravityData) = !phi0
fun getAcc0 ({acc0,...}:gravityData) = !acc0
fun getAI ({ai,...}:gravityData) = !ai
fun getDR ({dr,...}:gravityData) = !dr

fun setPhi0 ({phi0,...}:gravityData) phi0' = phi0 := phi0'
fun setAcc0 ({acc0,...}:gravityData) acc0' = acc0 := acc0'
fun setAI ({ai,...}:gravityData) ai' = ai := ai'
fun setDR ({dr,...}:gravityData) dr' = dr := dr'

(* Should we subdivide a node? *)
fun subdivide (node:body RegionTree.readOnlyNode)
	      (dsq:real,tolsq:real,gd:gravityData) =
    if (RegionTree.isLeaf node) then
	false
    else
	case RegionTree.getData (SOME node) of
	    NONE => false
	  | SOME body =>
	    let
		val pos = Body.getPos body
		val pos0 = getPos0 gd
		val dr = Point.sub (pos,pos0)
		val drsq = Point.dot (dr,dr)
	    in
		(setDR gd dr;
		 tolsq * drsq < dsq)
	    end

(* Compute single body-body interaction *)
fun twoBodies (body:body,gd:gravityData) =
    if Body.getID body = getSkipID gd then
        (* Don't interact body with itself *)
	()
    else
	let
	    val acc0 = getAcc0 gd
	    val dr = Point.sub (Body.getPos body,getPos0 gd)
	    val drsq = Point.dot (dr,dr) + eps * eps
	    val drabs = Math.sqrt drsq
	    val phi0 = getPhi0 gd
	    val phii = (Body.getMass body) / drabs
	    val mor3 = phii / drsq
	in
	    (setPhi0 gd (phi0 - phii);
	     let
		 val ai = Point.muls (dr,mor3)
	     in
		 (setAI gd ai;
		  setAcc0 gd (Point.add (acc0,ai)))
	     end;
	     setDR gd dr)
	end
		   
(* Recursively compute gravity induced by tree *)
fun bodyTree (tree:body RegionTree.readOnlyTree,
	      rsize:real,gd:gravityData) =
    let
	val tolsq = tol*tol
	fun computeGravity' (node:body RegionTree.readOnlyNode)
			    (dsq:real) =
	    if subdivide node (dsq,tolsq,gd) then
		let
		    fun processChild node =
			computeGravity' node (dsq / 4.0)
		in
		    case RegionTree.getChildren node of
			NONE => ()
		      | SOME children => Array.app 
					     (Util.optApp processChild)
					     children
		end
	    else
		case RegionTree.getData (SOME node) of
		    SOME body => twoBodies (body,gd)
		  | NONE => ()
    in
	case RegionTree.getRoot tree of
	    SOME node => computeGravity' node (rsize * rsize)
	  | NONE => ()
    end

(* Compute interaction for all bodies *)
fun allBodies (tree:body RegionTree.readOnlyTree,
	       rsize:real,
	       bodies:body option Array.array,
	       nstep:int) =
    let
	val newBodies = Array.array (Array.length bodies,NONE:body option)
	fun oneBody (i,NONE:body option) = ()
	  | oneBody (i,(SOME body):body option) =
	    let
		val dthf = 0.5 * dtime
		val acc1 = Body.getAcc body
		val gd = {skipID = Body.getID body,
			  pos0 = Body.getPos body,
			  phi0 = ref 0.0,
			  acc0 = ref Point.zero,
			  ai = ref Point.zero,
			  dr = ref Point.zero}:gravityData
	    in
		(bodyTree (tree,rsize,gd);
		 let
		     val acc0 = getAcc0 gd
		     val newBody = if nstep > 0 then
				       let
					   val phi0 = getPhi0 gd
					   val dacc = Point.sub (acc0,acc1)
					   val dvel = Point.muls (dacc,dthf)
					   val vel = Point.add (Body.getVel body,dvel)
				       in
					   Body.updateVelAccPhi body (vel,acc0,phi0)
				       end
				   else
				       Body.updateAcc body acc0
		 in
		     Array.update (newBodies,i,SOME newBody)
		 end)
	    end
    in
	(Array.appi oneBody bodies;
	 newBodies)
    end

(* Compute gravity on particles *)
fun computeForces (tree:Tree.t,
		   regionTree:body RegionTree.readOnlyTree,
		   nstep:int) =
	 let
	     val bodies = Tree.getBodies tree
	     val rsize = Tree.getRsize tree
	     val newBodies = allBodies (regionTree,
					rsize,bodies,nstep)
	 in
	     Tree.setBodies tree newBodies
	 end

(* Update particle positions *)
fun updatePositions (tree:Tree.t) =
    let
	val bodies = Tree.getBodies tree
	fun oneBody body:body =
	    let
		val acc = Body.getAcc body
		val vel = Body.getVel body
		val pos = Body.getPos body
		val dthf = 0.5 * dtime
		val dvel = Point.muls (acc,dthf)
		val vel1 = Point.add (vel,dvel)
		val dpos = Point.muls (vel1,dtime)
		val pos = Point.add (pos,dpos)
		val vel = Point.add(vel1,dvel)
	    in
		Body.updatePosVel body (pos,vel)
	    end
    in
	Array.modify (Util.opt oneBody) bodies
    end

end
