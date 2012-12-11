structure Body =
struct

type body = {ID:int,
	     mass:real,
	     pos:Point.t,
	     vel:Point.t,
	     acc:Point.t,
	     phi:real}

fun getID ({ID,...}:body) = ID
fun getMass ({mass,...}:body) = mass
fun getPos ({pos,...}:body) = pos
fun getVel ({vel,...}:body) = vel
fun getAcc ({acc,...}:body) = acc

fun new {ID:int,
	 mass:real,
	 pos:Point.t,
	 vel:Point.t} =
    {ID=ID,
     mass=mass,
     pos=pos,
     vel=vel,
     acc=Point.zero,
     phi=0.0}

fun updateMasPos {ID,mass,pos,vel,acc,phi} (mass',pos') =
    {ID=ID,mass=mass',pos=pos',vel=vel,acc=acc,phi=phi}

fun updateMassPosOpt (SOME body) (mass',pos') = SOME (updateMasPos body (mass',pos'))
  | updateMassPosOpt NONE (mass',pos') = SOME {ID= ~1,mass=mass',pos=pos',
					       vel=Point.zero,
					       acc=Point.zero,phi=0.0}

fun updateAcc ({ID,mass,pos,vel,acc,phi}) (acc') =
    {ID=ID,mass=mass,pos=pos,vel=vel,acc=acc',phi=phi}

fun updateVelAccPhi ({ID,mass,pos,vel,acc,phi}) (vel',acc',phi') =
    {ID=ID,mass=mass,pos=pos,vel=vel',acc=acc',phi=phi'}

fun updatePosVel ({ID,mass,pos,vel,acc,phi}) (pos',vel') =
    {ID=ID,mass=mass,pos=pos',vel=vel',acc=acc,phi=phi}

fun normalize (cmr,cmv)
	      {ID,mass,pos,vel,acc,phi} =
    {ID=ID,
     mass=mass,
     pos=Point.sub (pos,!cmr),
     vel=Point.sub (vel,!cmv),
     acc=acc,
     phi=phi}

fun toString {ID,mass,pos,vel,acc,phi} =
    "Body " ^ (Int.toString ID) ^ ": pos=" ^ (Point.toString pos)    
    
fun printBodies bodies =
    if (!Util.testMode) then
	Array.app (Util.optApp (fn x => print ((toString x) ^ "\n"))) bodies
    else
	()


end
