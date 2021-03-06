module SML.ArraySlice

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

let makeSlice (arr, i, sz, max) =
  let sz =
      match sz with
          Some sz ->
              let _ = checkRange (i, max - sz)
              sz
        | None    -> max
  { array = arr; start = i; size = sz }

let slice (arr, i, sz) =
  let len = Array.length arr
  makeSlice (arr, i, sz, len)

let full arr = slice (arr, 0, None)

let subslice (sl, i, sz) =
  let start = getStart sl
  let max = start + getSize sl
  makeSlice (getArray sl, start + i, sz, max)

let length sl = getSize sl

let sub (sl, i) =
  let _ = checkRange (i, (getSize sl) - 1)
  SML.Array.sub (getArray sl, (getStart sl) + i) 

let update (sl, i, elt) =
  let _ = checkRange (i, (getSize sl) - 1)
  SML.Array.update (getArray sl, (getStart sl) + i, elt)
