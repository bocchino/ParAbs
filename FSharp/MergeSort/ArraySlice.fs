module AS

type ArraySliceT =
  struct
    val arr : int array
    val off : int
    val len : int
    new (x : int array, y : int, z : int) = { arr = x; off = y; len = z }
  end

module ArraySlice =
  let full (a : int array) : ArraySliceT =
      new ArraySliceT(a, 0, a.Length)
  let slice (a : int array) (i : int) (len : int) : ArraySliceT =
      new ArraySliceT(a, i, len)
  let subslice (a : ArraySliceT) (i : int) (len : int) : ArraySliceT =
      new ArraySliceT(a.arr, i + a.off, len)
  let length (a : ArraySliceT) : int =
      a.len
  let sub (a : ArraySliceT) (idx : int) : int =
      Array.get a.arr (a.off + idx)
  let update (a : ArraySliceT) (idx : int) (nval : int) : Unit =
      Array.set a.arr (a.off + idx) nval

