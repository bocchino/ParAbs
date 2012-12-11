module ArraySlice

type 'a slice = {
  array : 'a array;
  start : int;
  size : int
}

exception Subscript

let getArray { array=array; start=_; size=_ } = array

let getStart { array=_; start=start; size=_ } = start

let getSize { array=_; start=_; size=size } = size

let checkRange (i,bound) =
  if i < 0 || i > bound then
    raise Subscript
  else ()

let slice (arr, i, sz) =
  let len = Array.length arr
  let makeSlice (arr, i, sz) =
    let _ = checkRange (i, len - 1)
    let _ = checkRange (sz, len)
    { array = arr; start = i; size = sz }
  match sz with
    Some sz -> makeSlice (arr, i, sz)
  | None    -> makeSlice (arr, i, len)

let full arr = slice (arr, 0, None)

let subslice (sl, i, sz) =
  slice (getArray sl, (getStart sl) + i, sz)

let length sl = getSize sl

let sub (sl, i) =
  Array.get (getArray sl) ((getStart sl) + i) 

let update (sl, i, elt) =
  Array.set (getArray sl) ((getStart sl) + i) elt
