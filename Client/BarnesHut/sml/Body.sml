structure Body =
struct

type body = {id:int,
	     mass:real,
	     pos:Point.t,
	     vel:Point.t,
	     acc:Point.t,
	     phi:real}

fun getId {id,mass,pos,vel,acc,phi} = id
fun getMass {id,mass,pos,vel,acc,phi} = mass
fun getPos {id,mass,pos,vel,acc,phi} = pos

fun new {id:int,
	 mass:real,
	 pos:Point.t,
	 vel:Point.t} =
    {id=id,
     mass=mass,
     pos=pos,
     vel=vel,
     acc=Point.zero,
     phi=0.0}

fun updateMassPos (SOME {id,mass,pos,vel,acc,phi}) (mass',pos') =
    SOME {id=id,mass=mass',pos=pos',vel=vel,acc=acc,phi=phi}
  | updateMassPos NONE _ = NONE

fun normalize (cmr,cmv)
	      {id,mass,pos,vel,acc,phi} =
    {id=id,
     mass=mass,
     pos=Point.sub (pos,!cmr),
     vel=Point.sub (vel,!cmv),
     acc=acc,
     phi=phi}

fun toString {id,mass,pos,vel,acc,phi} =
    "Body: pos=" ^ (Point.toString pos)    
    
fun printBodies bodies =
    if (!Util.testMode) then
	Array.app (Util.optApp (fn x => print ((toString x) ^ "\n"))) bodies
    else
	()


end
