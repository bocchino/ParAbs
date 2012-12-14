module Body

type body = {ID:int;
             mass:double;
             pos:Point.t;
             vel:Point.t;
             acc:Point.t;
             phi:double}


let getID   { ID=ID; mass=mass; pos=pos; vel=vel; acc=acc; phi=phi} = ID
let getMass { ID=ID; mass=mass; pos=pos; vel=vel; acc=acc; phi=phi} = mass
let getPos  { ID=ID; mass=mass; pos=pos; vel=vel; acc=acc; phi=phi} = pos
let getVel  { ID=ID; mass=mass; pos=pos; vel=vel; acc=acc; phi=phi} = vel
let getAcc  { ID=ID; mass=mass; pos=pos; vel=vel; acc=acc; phi=phi} = acc

let create (ID,mass,pos,vel) =
    {ID=ID;
     mass=mass;
     pos=pos;
     vel=vel;
     acc=Point.zero;
     phi=0.0}

let updateMassPos (body:body) (mass',pos') =
    { body with mass=mass';pos=pos' }

let updateMassPosOpt body (mass',pos') =
    match body with
        Some body -> Some (updateMassPos body (mass',pos'))
      | None      -> Some {ID= -1;mass=mass';pos=pos';
                           vel=Point.zero;
                           acc=Point.zero;phi=0.0}

let updateAcc (body:body) acc' =
    { body with acc=acc' }

let updateVelAccPhi (body:body) (vel',acc',phi') =
    { body with vel=vel';acc=acc';phi=phi' }

let updatePosVel (body:body) (pos',vel') =
    { body with pos=pos';vel=vel' }

let normalize (cmr,cmv)
              body: body =
    let pos = getPos body
    let vel = getVel body
    { body with pos=Point.sub (pos,!cmr); vel=Point.sub (vel,!cmv) }

let toString (body:body) =
    let ID = getID body
    let pos = getPos body
    "Body " + (sprintf "%d" ID) + ": pos=" + (Point.toString pos)
    
let printBodies bodies =
    if (!Util.outputMode) then
        let printFn = fun x -> Util.print ((toString x) + "\n")
        SML.Array.app (Util.optApp printFn) bodies
    else
        ()

