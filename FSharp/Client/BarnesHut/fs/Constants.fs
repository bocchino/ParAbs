module Constants

let NDIM = 3
let IMAX_SHIFT = 8 * 4 - 2

(* Highest bit of int coord *)
let IMAX = 1 <<< IMAX_SHIFT
let IMAX_REAL = float IMAX

let PI = 3.14159265358979323846
let TWO_PI = 6.28318530717958647693
let FOUR_PI = 12.56637061435917295385
let HALF_PI = 1.57079632679489661923
let FRTHRD_PI = 4.18879020478639098462

(* Mass cutoff *)
let MFRAC = 0.999

(* Subcells per cell *)
let NSUB = 1 <<< NDIM

(* Potential softening parameter *)
let eps = 0.05

(* Accuracy parameter: 0.0 => exact *)
let tol = 1.00

let NSTEPS = 10

(* Ratio of cells/bodies allocated *)
let fcells = 2.0

(* Timestep for leapfrog integrator *)
let dtime = 0.025

(* Time to stop calculation *)
let tstop = 2.0


