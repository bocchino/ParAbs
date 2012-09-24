structure Constants =
struct

val NDIM = 3
val IMAX_SHIFT = 8 * 4 - 2

(* Highest bit of int coord *)
val IMAX = Word32.<< (Word32.fromInt 1,Word.fromInt (8 * 4 - 2))

val PI = 3.14159265358979323846
val TWO_PI = 6.28318530717958647693
val FOUR_PI = 12.56637061435917295385
val HALF_PI = 1.57079632679489661923
val FRTHRD_PI = 4.18879020478639098462

(* Mass cutoff *)
val MFRAC = 0.999

(* Subcells per cell *)
val NSUB = Word32.toInt (Word32.<< (Word32.fromInt 1,Word.fromInt NDIM))

(* Potential softening parameter *)
val eps = 0.05

(* Accuracy parameter: 0.0 => exact *)
val tol = 1.00

val NSTEPS = 10

(* Ratio of cells/bodies allocated *)
val fcells = 2.0

(* Timestep for leapfrog integrator *)
val dtime = 0.025

(* Time to stop calculation *)
val tstop = 2.0

end
