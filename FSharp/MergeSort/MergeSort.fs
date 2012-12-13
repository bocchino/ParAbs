
open System
open AS
open DAS

let MERGE_SIZE = 0x400
let QUICK_SIZE = 0x800
let INSERTION_SIZE = 0x400

let seqMerge (a : ArraySliceT) (b : ArraySliceT) (out : ArraySliceT) : ArraySliceT =
    let mutable aPos = 0
    let aLen = ArraySlice.length a
    let mutable bPos = 0
    let bLen = ArraySlice.length b
    let mutable k = 0
    while aPos < aLen && bPos < bLen do
          if ArraySlice.sub a aPos < ArraySlice.sub b bPos then
             ArraySlice.update out k (ArraySlice.sub a aPos)
             aPos <- aPos + 1
          else
             ArraySlice.update out k (ArraySlice.sub b bPos)
             bPos <- bPos + 1
          k <- k + 1
    while aPos < aLen do
          ArraySlice.update out k (ArraySlice.sub a aPos)
          aPos <- aPos + 1
          k <- k + 1
    while bPos < bLen do
          ArraySlice.update out k (ArraySlice.sub b bPos)
          bPos <- bPos + 1
          k <- k + 1
    out

let findSplit (v : int) (a : ArraySliceT) : int =
    let mutable low = 0
    let mutable high = ArraySlice.length a
    while low < high do
          let mid = low + ((high - low) / 2)
          if v <= ArraySlice.sub a mid then
             high <- mid
          else
             low <- mid + 1
    high

let rec merge (arrays : slices) : Unit =
   match (DisjointArraySlices.getArrays arrays) with
      | [a; b; out] ->
        if ArraySlice.length a <= MERGE_SIZE then
          seqMerge a b out
          ()
        else
          let aHalf = (ArraySlice.length a) / 2
          let bSplit = findSplit (ArraySlice.sub a aHalf) b
          let halves = DisjointArraySlices.transpose (DisjointArraySlices.split arrays [[aHalf]; [bSplit]; [aHalf + bSplit]])
          DisjointArraySlices.apply merge halves
        | _ -> Console.WriteLine("bad argument"); ()

let insertionSort (a : ArraySliceT) : ArraySliceT =
    let mutable i = 1
    let mutable j = 0
    let mutable t = 0
    let len = ArraySlice.length a
    while i < len do
          t <- ArraySlice.sub a i
          j <- i - 1
          while j >= 0 && ArraySlice.sub a j > t do
                ArraySlice.update a (j + 1) (ArraySlice.sub a j)
                j <- j - 1
          ArraySlice.update a (j + 1) t
          i <- i + 1
    a

let rec quickSort (a : ArraySliceT) : ArraySliceT =
    if (ArraySlice.length a) <= INSERTION_SIZE then
       insertionSort a
    else
       let lo = 0
       let hi = (ArraySlice.length a) - 1
       let mid = (lo + hi) / 2
       let mutable left = lo + 1
       let mutable right = hi - 1
       if ArraySlice.sub a lo > ArraySlice.sub a mid then
          let t = ArraySlice.sub a lo
          ArraySlice.update a lo (ArraySlice.sub a mid)
          ArraySlice.update a mid t
       if ArraySlice.sub a mid > ArraySlice.sub a hi then
          let t = ArraySlice.sub a mid
          ArraySlice.update a mid (ArraySlice.sub a hi)
          ArraySlice.update a hi t
          if ArraySlice.sub a lo > ArraySlice.sub a mid then
             let t = ArraySlice.sub a lo
             ArraySlice.update a lo (ArraySlice.sub a mid)
             ArraySlice.update a mid t
       let partition = ArraySlice.sub a mid
       let mutable don = false
       while not don do
             while ArraySlice.sub a right > partition do
                   right <- right - 1
             while left < right && ArraySlice.sub a left <= partition do
                   left <- left + 1
             if left < right then
                let t = ArraySlice.sub a left
                ArraySlice.update a left (ArraySlice.sub a right)
                ArraySlice.update a right t
             else
                don <- true
       quickSort (ArraySlice.subslice a lo (left + 1))
       quickSort (ArraySlice.subslice a (left + 1) (hi - left))


let sort (a : ArraySliceT) : Unit =
    let splitFirst (idx : int) (arrs : slices) =
                  DisjointArraySlices.flatten (DisjointArraySlices.split arrs [[idx]; []])
    let rec sortp (arrs : slices) : Unit =
        let a = match (DisjointArraySlices.getArrays arrs) with
                 | [a; _] -> a
                 | _ -> Console.WriteLine("Bad Argument"); ArraySlice.full Array.empty
        let len = ArraySlice.length a
        let q = len / 4
        let quarterIdxs = [q; 2 * q; 3 * q]
        let quarters = DisjointArraySlices.transpose (DisjointArraySlices.split arrs [quarterIdxs; quarterIdxs])
        let halves = DisjointArraySlices.transpose (DisjointArraySlices.split arrs [[2 * q]; [2 * q]])
        if len <= QUICK_SIZE then
          quickSort a
          ()
        else
          DisjointArraySlices.apply sortp quarters
          DisjointArraySlices.apply (merge << (splitFirst q)) halves
          merge (splitFirst (2 * q) (DisjointArraySlices.rev arrs))
    let arrs = DisjointArraySlices.add (DisjointArraySlices.fromArray a) [(ArraySlice.length a, 0)]
    sortp arrs


let randArray (size : int) : ArraySliceT =
    let rnd = System.Random()
    let arr = Array.init size (fun index -> rnd.Next(0, 10000))
    ArraySlice.full arr

let checkSorted (a : ArraySliceT) =
    printfn "%A" a
    for i in 1 .. (ArraySlice.length a) - 1 do
        if ArraySlice.sub a (i - 1) > ArraySlice.sub a i then
          Console.WriteLine("not sorted!")

let DEFAULT_SIZE = 0x1000000

let Main =
    let t1 = randArray DEFAULT_SIZE
    printfn "%A" t1
    sort t1
    checkSorted t1
