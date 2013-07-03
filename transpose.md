## Generalising Transpose

Lazy vectors, also called pull-arrays, are implementations of flat
vectors consisting of an integer, specifying the size of the vector,
and a function taking an index as an argument (zero-based) and
returning the value of the array.

Lazy vectors can be used as the fundamental building block for
constructing Lazy multi-dimensional arrays, by representing a
multi-dimensional array as a pair of two lazy vectors, one specifying
the shape of the array and one holding the values of the array.

The transpose operation on two-dimensional arrays can be defined as
follows - using SML - for simplicity, the shape vector is implemented
as an integer list:

```sml
  type 'a array = int list * (int * (int -> 'a))
  fun transpose (a : 'a array) : 'a array =
    case a of
      ([M,N], (sz,f)) => (* invariant: sz = M*N *)
        ([N,M], (sz,
                 fn i =>
                  let val t = M * N - 1
                  in if i = t then f i
                     else f ((N*i) mod t)
                  end)
        )
     | _ => raise Fail "assuming 2-dimensional array"
  fun pp_list l = String.concatWith " " (map Int.toString l)
  fun pp_pa pp (sz,f) =
    let fun iter n a = if n < 0 then a
                       else iter (n-1) (pp (f n) :: a)
    in String.concatWith " " (iter (sz-1) nil)
    end
  fun pp p (sh,v) = "(" ^ pp_list sh ^ "){" ^ pp_pa p v ^ "}"
  fun ppi (a : int array) : string = pp Int.toString a
  fun reshape l (_, v) = (l,v)
  fun iota n = ([n],(n,fn x => x))
  fun fromList l : int array = ([length l], (length l,fn i => List.nth(l,i)))
  val a = reshape [2,3] (iota 6)
  val _ = print ("a = " ^ ppi a ^ "\n")
  val b = transpose a
  val _ = print ("b = " ^ ppi b ^ "\n")
```
Here is the result of compiling and executing the above program:

    bash-3.2$ mlkit transpose.sml 
    [reading source file:        transpose.sml]
    [wrote X86 code file:        MLB/RI_GC/transpose.sml.s]
    [wrote X86 code file:        MLB/RI_GC/base-link_objects.s]
    [wrote executable file:      run]
    bash-3.2$ ./run
    a = (2 3){0 1 2 3 4 5}
    b = (3 2){0 3 1 4 2 5}

In APL, transposition is generalised to multi-dimensional arrays by
reversing the shape of the argument array. Here is the output of a
tryapl.org session:

```apl
     ⍴ 2 3 4 5 ⍴ ⍳ 120
2 3 4 5
      ⍴ ⍉ 2 3 4 5 ⍴ ⍳ 120
5 4 3 2
```

Now the question is how one may extend the definition of transpose to
cover also higher-dimension arrays?

**Note:** Another approach to implementing lazy multi-dimensional arrays
is to represent them as a pair of a shape vector and a function taking
an index vector and returning the corresponding array value. This
approach would simplify transposition on the cost of complicating
reshape.