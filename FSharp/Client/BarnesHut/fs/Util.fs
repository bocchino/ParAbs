module Util

open System

let A = 16807.0
let M = 2147483647.0

(* Are we in output mode? *)
let outputMode = ref false

let rand (seed:double) =
    let t = A * seed + 1.0
    t - (M * (Math.Floor (t / M)))

let xrand (xl,xh,r) =
    xl + (xh - xl) * r / 2147483647.0

let logging = ref false

let print (s:string) = Console.Write s

let log s = print s

let opt f =
    fun x ->
        match x with
            Some s -> Some (f s)
          | None -> None

let optApp f =
    fun x ->
        match x with
            Some s -> f s
          | None -> ()

let printOpt s =
    match s with
        Some s -> print s
      | None     -> print "None"

let printRet () = print "\n"

let realToString r = sprintf "%.6f" r

let err str =
    eprintfn "%s" ("barnes-hut: " + str)

