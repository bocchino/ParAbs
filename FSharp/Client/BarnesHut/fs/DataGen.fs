module DataGen

open Body
open Constants

let uniformTestData (bodies,segNum,cmr,cmv) =
    let nbody = Array.length bodies
    let r = ref 0.0
    let v = ref 0.0
    let x = ref 0.0
    let y = ref 0.0
    let rsq = ref 0.0
    let rsc1 = ref 0.0
    let temp = ref 0.0
    let t1 = ref 0.0
    let coeff = 4.0
    let i = ref 0
    let seedFactor = segNum + 1
    let seed = ref (123.0 * (float seedFactor))
    let rad = ref 0.0
    let nbodyx = nbody / 32
    let rsc = 3.0 * PI / 16.0
    let vsc = sqrt (1.0 / rsc)
    let pos = ref Point.zero
    let vel = ref Point.zero
    let continuing = ref true
    let rockmass = 1.0 / ((float nbody) / 32.0)
    let start = nbodyx * segNum
    (while !i < nbodyx do
         (seed := Util.rand (!seed);
          t1 := Util.xrand (0.0,MFRAC,!seed);
          temp := SML.Math.pow(!t1,-2.0/3.0) - 1.0;
          r := 1.0 / (sqrt (!temp));

          let k = ref 0
          let posList = ref ([]:float list)
          while !k < NDIM do
              (seed := Util.rand (!seed);
               r := Util.xrand (0.0,MFRAC,!seed);
               posList := (coeff * !r) :: (!posList);
               k := !k + 1);
          posList := List.rev (!posList);
          pos := Point.fromList (!posList);

          cmr := Point.add (!cmr, !pos);

          continuing := true;
          while !continuing do
              (seed := Util.rand (!seed);
               x := Util.xrand (0.0,1.0,!seed);
               seed := Util.rand (!seed);
               y := Util.xrand(0.0,0.1,!seed);
               continuing := !y > (!x * !x * SML.Math.pow (1.0 - !x * !x,3.5)));

          v := (sqrt 2.0) * !x / (SML.Math.pow (1.0 + !r * !r, 0.25));
          rad := vsc * !v;

          continuing := true;
          while !continuing do
               let k = ref 0
               let velList = ref ([]:float list)
               (while !k < NDIM do
                    (seed := Util.rand (!seed);
                     velList := (Util.xrand (-1.0,1.0,!seed)) :: (!velList);
                     k := !k + 1);
                velList := List.rev (!velList);
                vel := Point.fromList (!velList);
                rsq := Point.dot (!vel,!vel);
                continuing := !rsq > 1.0);
          rsc1 := !rad / (sqrt (!rsq));
          vel := Point.muls (!vel,!rsc1);
          cmv := Point.add (!cmv,!vel);

          let idx = start + (!i);
          let body = Body.create (idx,rockmass,!pos,!vel);
          SML.Array.update (bodies,idx,Some body);

          i := !i + 1))
