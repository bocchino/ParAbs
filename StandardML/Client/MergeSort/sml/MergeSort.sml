structure MergeSort =
struct

open DisjointArraySlices

exception BadArgument

val MERGE_SIZE = 0x400
val QUICK_SIZE = 0x800
val INSERTION_SIZE = 0x400

fun seqMerge (a,b,out) =
    let
	val aPos = ref 0
	val aLen = ArraySlice.length a
	val bPos = ref 0
	val bLen = ArraySlice.length b
	val k = ref 0
    in
	(while !aPos < aLen andalso !bPos < bLen do
	    (if ArraySlice.sub (a,!aPos) < ArraySlice.sub (b,!bPos) then
		 (ArraySlice.update (out,!k,ArraySlice.sub (a,!aPos));
		  aPos := !aPos + 1)
	     else
		 (ArraySlice.update (out,!k,ArraySlice.sub (b,!bPos));
		  bPos := !bPos + 1);
	     k := !k + 1);
	 while (!aPos < aLen) do
	     (ArraySlice.update (out,!k,ArraySlice.sub (a,!aPos));
	      aPos := !aPos + 1;
	      k := !k + 1);
	 while (!bPos < bLen) do
	     (ArraySlice.update (out,!k,ArraySlice.sub (b,!bPos));
	      bPos := !bPos + 1;
	      k := !k + 1))
    end

fun findSplit (v,a) =
    let
	val low = ref 0
	val high = ref (ArraySlice.length a)
    in
	(while !low < !high do
	     let
		 val mid = !low + ((!high - !low) div 2)
	     in
		 if v <= ArraySlice.sub (a,mid) then
		     high := mid
		 else
		     low := mid + 1
	     end;
	 !high)
    end

fun merge arrays =
    case getArrays arrays of
	[a,b,out] => 
	if ArraySlice.length a <= MERGE_SIZE then
	    seqMerge (a,b,out)
	else
	    let
		val aHalf = (ArraySlice.length a) div 2
		val bSplit = findSplit (ArraySlice.sub (a,aHalf), b)
		val halves = transpose (split arrays
					      [[aHalf],[bSplit],[aHalf + bSplit]])
	    in
		apply merge halves
	    end
      | _ => raise BadArgument

fun insertionSort a = 
    let
	val i = ref 1
	val j = ref 0
	val t = ref 0
	val len = ArraySlice.length a
    in
	while !i < len do
	    (t := ArraySlice.sub (a,!i);
	     j := !i - 1;
	     while !j >= 0 andalso ArraySlice.sub (a,!j) > !t do
		 (ArraySlice.update (a,!j + 1,ArraySlice.sub (a,!j));
		  j := !j - 1);
	     ArraySlice.update (a,!j+1,!t);
	     i := !i + 1)
    end

fun quickSort a =
    if ArraySlice.length a <= INSERTION_SIZE then
	insertionSort a
    else 
	let
	    val lo = 0
	    val hi = ArraySlice.length a - 1
	    val mid = (lo + hi) div 2
	    val left = ref (lo + 1)
	    val right = ref (hi - 1)
	in
	    (if ArraySlice.sub (a,lo) > ArraySlice.sub (a,mid) then
		let
		    val t = ArraySlice.sub (a,lo)
		in
		    (ArraySlice.update (a,lo,ArraySlice.sub (a,mid));
		     ArraySlice.update (a,mid,t))
		end
	     else ();
	     if ArraySlice.sub (a,mid) > ArraySlice.sub (a,hi) then
		 let
		     val t = ArraySlice.sub (a,mid)
		 in
		     (ArraySlice.update (a,mid,ArraySlice.sub(a,hi));
		      ArraySlice.update (a,hi,t);
		      if ArraySlice.sub (a,lo) > ArraySlice.sub (a,mid) then
			  let
			      val t = ArraySlice.sub (a,lo)
			  in
			      (ArraySlice.update(a,lo,ArraySlice.sub(a,mid));
			       ArraySlice.update(a,mid,t))
			  end
		      else
			  ())
		 end
	     else ();
	     let
		 val partition = ArraySlice.sub (a,mid)
		 val done = ref false
	     in
		 while not (!done) do
		     (while ArraySlice.sub (a,!right) > partition do
			  right := !right - 1;
		      while !left < !right andalso ArraySlice.sub (a,!left) <= partition do
			  left := !left + 1;
		      if !left < !right then
			  let
			      val t = ArraySlice.sub (a,!left)
			  in
			      (ArraySlice.update (a,!left,ArraySlice.sub (a,!right));
			       ArraySlice.update (a,!right,t))
			  end
		      else
			  done := true)
	     end;
	     quickSort(ArraySlice.subslice (a,lo,SOME (!left + 1)));
	     quickSort(ArraySlice.subslice (a,!left + 1,SOME (hi - !left))))
	end

fun sort arr =
    let 
	fun splitFirst idx arrs =
	    flatten (split arrs [[idx],[]])
	fun sort' arrs =
	    let
		val a = case getArrays arrs of
			    [a,_] => a
			  | _     => raise BadArgument
		val len = ArraySlice.length a
		val q = len div 4
		val quarterIdxs = [q,2*q,3*q]
		val quarters = transpose (split arrs [quarterIdxs,quarterIdxs])
		val halves = transpose (split arrs [[2*q],[2*q]])
	    in
		if len <= QUICK_SIZE then
		    quickSort a
		else
		    (apply sort' quarters;
		     apply (merge o (splitFirst q)) halves;
		     merge (splitFirst (2*q) (rev arrs)))
	    end
	val arrs = add (fromArray arr,[(Array.length arr,0)])
    in
	sort' arrs
    end

fun randArray rand size =
    let
	val i = ref 0
	val a = Array.array (size,0)
    in
	(while !i < size do
	     (Array.update (a,!i,Random.randRange (1,size) rand);
	      i := !i + 1);
	 a)
    end

exception Unsorted of string

fun checkSorted a =
    let
	val i = ref 0
    in
	while !i < Array.length a - 1 do
	    (if Array.sub (a,!i) <= Array.sub (a,!i+1) then
		 ()
	     else
		 raise Unsorted ("Unsorted at " ^ (Int.toString (!i)) ^ ": " ^
		  (Int.toString (Array.sub (a,!i))) ^ "/" ^
		       (Int.toString (Array.sub (a,!i + 1))));
	     i := !i + 1)
    end

val MAX =
    case Int32.maxInt of
        SOME m => Int32.toInt(m div 2)
      | NONE => 0xFFFFFFF
val DEFAULT_SIZE=0x10000

fun main (name,args) =
    let 
	val sizeOption = case args of
			     arg :: _ => Int.fromString arg
			   | _        => NONE
	val size = case sizeOption of
		       SOME n => n
		     | NONE => DEFAULT_SIZE
	val rand = Random.rand (IntInf.toInt(Time.toSeconds(Time.now())
                                             mod IntInf.fromInt(MAX)),0)
	val a = randArray rand size
    in	       
	(sort a;
	 checkSorted a;
	 OS.Process.success)
	handle Unsorted s => (print (s ^ "\n"); OS.Process.failure)
	     | _ => OS.Process.failure
    end

end
