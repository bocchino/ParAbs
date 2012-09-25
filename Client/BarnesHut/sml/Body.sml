structure Body =
struct

type body = {mass:real,
	     pos:Point.t,
	     vel:Point.t,
	     acc:Point.t,
	     phi:real}

fun getMass {mass,pos,vel,acc,phi} = mass
fun getPos {mass,pos,vel,acc,phi} = pos

fun new {mass:real,
	 pos:Point.t,
	 vel:Point.t} =
    {mass=mass,
     pos=pos,
     vel=vel,
     acc=Point.zero,
     phi=0.0}

fun updateMassPos (SOME {mass,pos,vel,acc,phi}) (mass',pos') =
    {mass=mass',pos=pos',vel=vel,acc=acc,phi=phi}
  | updateMassPos NONE (mass,pos) =
    new {mass=mass,pos=pos,vel=Point.zero}

fun normalize (cmr,cmv)
	      {mass,pos,vel,acc,phi} =
    {mass=mass,
     pos=Point.sub (pos,!cmr),
     vel=Point.sub (vel,!cmv),
     acc=acc,
     phi=phi}

fun toString {mass,pos,vel,acc,phi} =
    "Body: pos=" ^ (Point.toString pos)    
    
fun printBodies bodies =
    if (!Util.testMode) then
	Array.app (Util.optApp (fn x => print ((toString x) ^ "\n"))) bodies
    else
	()


end
