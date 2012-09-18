(* An abstraction for divide-and-conquer updates on arrays *)

signature DISJOINT_ARRAY_SLICES =
sig

    exception BadArgument

    (* A list of disjoint array slices *)
    type 'a slices

    (* A list of lists of disjoint array slices *)
    type 'a partitions

    (* Create a list of disjoint array slices *)
    val slices : (int * 'a) list -> 'a slices
    val fromLists : 'a list list -> 'a slices
    val fromArray : 'a Array.array -> 'a slices

    (* Add fresh slices to an existing list *)
    val add : 'a slices * (int * 'a) list -> 'a slices

    (* Split each slice in the collection on the given indices *)
    val split : 'a slices -> int list list -> 'a partitions

    (* Transpose a list of lists of slices *)
    val transpose : 'a partitions -> 'a partitions

    (* Flatten a list of array lists into a single list *)
    val flatten : 'a partitions -> 'a slices

    (* Reverse a list of slices *)
    val rev : 'a slices -> 'a slices

    (* Apply a function in parallel to each element of the list *)
    val apply : ('a slices -> unit) -> 'a partitions -> unit

    (* Get the slices *)
    val getArrays : 'a slices -> 'a ArraySlice.slice list

    (* Get the list of slices *)
    val getPartitions : 'a partitions -> 'a slices list

end
