structure DisjointArraySlices : DISJOINT_ARRAY_SLICES =
struct

type 'a slices = 'a ArraySlice.slice list
type 'a partitions = 'a ArraySlice.slice list list

exception BadArgument

fun transpose ([]::_) = []
  | transpose rows    = map hd rows :: transpose (map tl rows)

fun flatten ls =
    List.concat ls

fun slices specs =
    List.map (ArraySlice.full o Array.array) specs

fun fromLists lists =
    List.map (ArraySlice.full o Array.fromList) lists

fun fromArray a = [ArraySlice.full a]

fun add (a,specs) = a @ (slices specs)

fun splitOne (slice,is) =
    let 
	val starts = 0 :: is
	val ends = is @ [(ArraySlice.length slice)]
	val lens = ListPair.map (op -) (ends,starts)
	fun mkSlice (start,len) = ArraySlice.subslice (slice,start,SOME len)
    in
	ListPair.map mkSlice (starts,lens)
    end

fun split sliceList isList =
    List.map splitOne (ListPair.zipEq (sliceList,isList))

fun rev arrs = List.rev arrs

fun apply f ps =
    (* TODO: Should be parallel *)
    List.app f ps

fun getArrays slices = slices

fun getPartitions partitions = partitions

end
