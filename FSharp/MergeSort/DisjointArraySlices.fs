module DAS

open AS

type slices = ArraySliceT list
type partitions = ArraySliceT list list

module DisjointArraySlices =
  let slice (specs : (int * int) list) : slices =
    let cre (x : (int * int)) : int array =
        let (a, b) = x
        Array.create a b
    List.map (ArraySlice.full << cre) specs
(*  let fromLists : ArraySliceT list list : ArraySlicesT list = *)
  let fromArray (arr : ArraySliceT) : slices =
      [arr]
  let add (s : slices) (n : (int * int) list) : slices =
      List.append s (slice n)
  let splitOne (s : ArraySliceT) (x : int list) : slices =
      let starts = 0 :: x
      let ends = List.append x [ArraySlice.length s]
      let opm (a : int) (b : int) : int = b - a
      let lengths = List.map2 opm starts ends
      let mkSlice (x : (int * int)) : ArraySliceT =
          let (st, l) = x
          ArraySlice.subslice s st l
      List.map mkSlice (List.zip starts lengths)
  let split (s : slices) (x : int list list) : partitions =
      let so (x : (ArraySliceT * int list)) : slices =
          let (a, b) = x
          splitOne a b
      List.map so (List.zip s x)
  let rec transpose (p : partitions) : partitions =
      if (List.isEmpty p) then
        []
      else
        List.map List.head p :: transpose (List.map List.tail p)
  let flatten (p : partitions) : slices =
      List.concat p
  let rev (s : slices) : slices =
      List.rev s
  let apply (a : (slices -> Unit)) (p : partitions) : Unit =
      List.map a p
      ()
  let getArrays (s : slices) : ArraySliceT list =
      s
  let getPartitions (p : partitions) : ArraySliceT list list =
      p
