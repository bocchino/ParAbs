structure Body =
struct

type body = {ID:int,
	     mass:real,
	     pos:Point.t,
	     vel:Point.t,
	     acc:Point.t,
	     phi:real}

fun getID {ID,mass,pos,vel,acc,phi} = ID
fun getMass {ID,mass,pos,vel,acc,phi} = mass
fun getPos {ID,mass,pos,vel,acc,phi} = pos
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

fun updateMassPos (SOME {ID,mass,pos,vel,acc,phi}) (mass',pos') =
    SOME {ID=ID,mass=mass',pos=pos',vel=vel,acc=acc,phi=phi}
  | updateMassPos NONE _ = NONE

fun updateVelAccPhi ({ID,mass,pos,vel,acc,phi}) (vel',acc',phi') =
    {ID=ID,mass=mass,pos=pos,vel=vel',acc=acc',phi=phi'}

fun normalize (cmr,cmv)
	      {ID,mass,pos,vel,acc,phi} =
    {ID=ID,
     mass=mass,
     pos=Point.sub (pos,!cmr),
     vel=Point.sub (vel,!cmv),
     acc=acc,
     phi=phi}

fun toString {ID,mass,pos,vel,acc,phi} =
    "Body: pos=" ^ (Point.toString pos)    
    
fun printBodies bodies =
    if (!Util.testMode) then
	Array.app (Util.optApp (fn x => print ((toString x) ^ "\n"))) bodies
    else
	()


end
