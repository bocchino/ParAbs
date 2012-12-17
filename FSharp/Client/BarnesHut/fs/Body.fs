module Body

type body =
  struct
    val ID:int
    val mass:double
    val pos:Point.t
    val vel:Point.t
    val acc:Point.t
    val phi:double
    new (a : int, b :double, c : Point.t, d: Point.t, e: Point.t, f : double) =
      {ID = a; mass = b; pos = c; vel = d; acc = e; phi = f; }
   end


let getID   (a : body) = a.ID;
let getMass (a : body) = a.mass;
let getPos  (a : body) = a.pos;
let getVel  (a : body) = a.vel;
let getAcc  (a : body) = a.acc;
let getPhi  (a : body) = a.phi

let create (ID,mass,pos,vel) =
    new body(ID, mass, pos, vel, Point.zero, 0.0)

let updateMassPos (body:body) (mass',pos') =
    create(getID body, mass', pos', getVel body)

let updateMassPosOpt body (mass',pos') =
    match body with
        Some body -> Some (updateMassPos body (mass',pos'))
      | None      -> Some (create (-1, mass',pos',Point.zero))

let updateAcc (body:body) acc' =
    new body(getID body, getMass body, getPos body, getVel body, acc', getPhi body)

let updateVelAccPhi (body:body) (vel',acc',phi') =
    new body(getID body, getMass body, getPos body, vel', acc', phi')

let updatePosVel (body:body) (pos',vel') =
    new body(getID body, getMass body, pos', vel', getAcc body, getPhi body)

let normalize (cmr,cmv)
              body: body =
    let pos = getPos body
    let vel = getVel body
    new body (getID body, getMass body, Point.sub(pos, !cmr), Point.sub(vel, !cmv), getAcc body, getPhi body)

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

