structure Body =
struct

type body = {mass:real,
	     pos:Point.t,
	     vel:Point.t,
	     acc:Point.t,
	     phi:real}

fun getPos ({mass,pos,vel,acc,phi}:body) = pos

fun new {mass:real,
	 pos:Point.t,
	 vel:Point.t} =
    {mass=mass,
     pos=pos,
     vel=vel,
     acc=Point.zero,
     phi=0.0}

fun normalize (cmr,cmv)
	      {mass,pos,vel,acc,phi} =
    {mass=mass,
     pos=Point.sub (pos,!cmr),
     vel=Point.sub (vel,!cmv),
     acc=acc,
     phi=phi}

fun toString {mass,pos,vel,acc,phi} =
    "Body: pos=" ^ (Point.toString pos)    
    

end
