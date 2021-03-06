structure DataGen =
struct

open Body
open Constants

fun uniformTestData (bodies,segNum,cmr,cmv) =
    let
	val nbody = Array.length bodies
	val r = ref 0.0
	val v = ref 0.0
	val x = ref 0.0
	val y = ref 0.0
	val rsq = ref 0.0
	val rsc1 = ref 0.0
	val temp = ref 0.0
	val t1 = ref 0.0
	val coeff = 4.0
	val i = ref 0
	val seedFactor = segNum + 1
	val seed = ref (123.0 * (Real.fromInt seedFactor))
	val rad = ref 0.0
	val nbodyx = nbody div 32
	val rsc = 3.0 * PI / 16.0
	val vsc = Math.sqrt (1.0 / rsc)
	val pos = ref Point.zero
	val vel = ref Point.zero
	val continue = ref true
	val rockmass = 1.0 / ((Real.fromInt nbody) / 32.0)
	val start = nbodyx * segNum
    in
	(while !i < nbodyx do
	     (seed := Util.rand (!seed);
	      t1 := Util.xrand (0.0,MFRAC,!seed);
	      temp := Math.pow (!t1, ~2.0/3.0) - 1.0;
	      r := 1.0 / (Math.sqrt (!temp));
	      
	      let
		  val k = ref 0
		  val posList = ref ([]:real list)
	      in
		  while !k < NDIM do
		      (seed := Util.rand (!seed);
		       r := Util.xrand (0.0,MFRAC,!seed);
		       posList := (coeff * !r) :: (!posList);
		       k := !k + 1);
		  posList := List.rev (!posList);
		  pos := Point.fromList (!posList)
	      end;
	      cmr := Point.add (!cmr, !pos);
						
	      continue := true;
	      while !continue do
		  (seed := Util.rand (!seed);
		   x := Util.xrand (0.0,1.0,!seed);
		   seed := Util.rand (!seed);
		   y := Util.xrand(0.0,0.1,!seed);
		   continue := !y > (!x * !x * Math.pow (1.0 - !x * !x,3.5)));

	      v := (Math.sqrt 2.0) * !x / (Math.pow (1.0 + !r * !r, 0.25));
	      rad := vsc * !v;

	      continue := true;
	      while !continue do
		   let
		       val k = ref 0
		       val velList = ref ([]:real list)
		   in
		       (while !k < NDIM do
			    (seed := Util.rand (!seed);
			     velList := (Util.xrand (~1.0,1.0,!seed)) :: (!velList);
			     k := !k + 1);
			velList := List.rev (!velList);
			vel := Point.fromList (!velList);
			rsq := Point.dot (!vel,!vel);
			continue := !rsq > 1.0)
		   end;
	      rsc1 := !rad / (Math.sqrt (!rsq));
	      vel := Point.muls (!vel,!rsc1);
	      cmv := Point.add (!cmv,!vel);

	      let
		  val idx = start + (!i)
	      in
		  Array.update (bodies,idx,SOME (Body.new {ID=idx,
							   mass=rockmass,
							   pos=(!pos),
							   vel=(!vel)}))
	      end;

	      i := !i + 1))

    end



end
