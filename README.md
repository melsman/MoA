## Vectors and Multi-dimensional Arrays

This software provides implementations of a series of vector and array
modules in Standard ML.

 * `vec.mlb`. Implementation of one-dimensional vectors. Several
   implementations are included, including a version based on pull and
   push arrays.

 * `ilvec.mlb`. Implementation of code-generational one-dimensional
   pull arrays. Some support for nested vectors.

 * `moa.mlb`. Implementation of multi-dimensional array
   calculus. Makes use of `vec.mlb`.

 * `ilmoa.mlb`. Implementation of code-generational
   multi-dimensional array calculus. Makes use of `ilvec.mlb`.

The "il" versions of the vector and array libraries are
implementations that generate residual intermediate language "C like"
code from the specification of the vector or array program. Contrary,
the non-"il" versions of the libraries are implementations for which
the vector and array operations are performed in ML itself. 

## One-dimensional vector implementations

The sources contain various basic vector library implementations that
are used as the basis for the MoA realization. The `vec/` folder
contains three implementations of the `VEC` signature:

  1. A reference vector implementation based on Standard ML lists.

  1. An implementation based on _pull arrays_, which consists of a
     number denoting the size of the array and a function for reading
     an element of the array, given an index. Pull arrays support map
     fusion and work well up-to a number of extensions, including drop
     and take functionality.

  1. An implementation based on both pull arrays and _push arrays_,
     which are continuation-based representations of arrays and dual to
     pull arrays in the sense that they support concatenation well,
     but drop and take operations not so well.

The `ilvec/` folder contains two different vector libraries that
support map fusion and generate residual (C-like) code. The second
implementation (`il2.mlb`) supports nesting of vectors and thus,
nested folds. As an example, let's first construct a matrix containing
the small multiplification table:

```sml
val mat = tabulate (I 10) (fn i => tabulate (I 10) (fn j => i * j))
```
                                   
Here the `I` function lifts integers into the vector expression
language, on which all calculations are performed. The lifting allows
operations on vector expressions to be residualized into the C-like
target language. Now, let's write some code for summing up all the
values in the table:

```sml
val m = foldr (fn (r, a) => foldr (ret o op +) a r) (I 0) mat
```

Here we see two nested folds, with the outer foldr folding over the
rows of the matrix with `I 0` as the base value for the
accumulator. The inner foldr adds all elements in a row to the outer
accumulator.

Here are the types of `foldr` and `tabulate`:

```sml
val foldr    : ('a t * 'b t -> 'b t M) -> 'b t -> 'a v -> 'b t M
val tabulate : INT -> (INT -> 'a t) -> 'a v
```

We see that `foldr` returns a monadic value, which we can "run" using
the `runM` function, which again generates a residual program
(slightly simplified):

```c
int n49 = 0;
for (int n52 = 0; n52 < 10; n52++) {
    int n53 = n49;
    for (int n55 = 0; n55 < 10; n55++) {
        n53 += ((9-n52)*(9-n55));
    }
    n49 = n53;
}
```

After executing the above code, the result is present in variable
`n49`.

## MoA - Multi-dimensional arrays

The `moa.mlb` and `ilmoa.mlb` libraries provide implementations of
a multi-dimentional array calculus based on the paper:

 * __G. Hains et L. M. R. Mullin__. _An algebra of multidimensional
   arrays_. Publication 783, DIRO, Departement d'Informatique et de
   Recherche Operationnelle, Universite de Montreal, 1991.

In essence, arrays are implemented as a product of two one-dimensional
vectors, where the first vector specifies the shape of the array
(ranks and dimensions) and where the second vector specifies the
content of the array, as contiguous elements.

We demonstrate the `ilmoa.mlb` library by example. Consider the
following signal processing program:

```sml
fun diff (signal (*:n*)) (*:n-1*) =
    rrotate (I 1) signal >>= (fn r => 
      sum Double (op -) signal r >>= (fn (r' (*:n*)) =>
      ret (drop (I 1) r')))
fun maxsv s v = mmap (fn x => max x s) v   (* scalar-vector max *)
fun minsv s v = mmap (fn x => min x s) v   (* scalar-vector min *)
fun prodsv s v = mmap (fn x => x * s) v    (* scalar-vector multiplication *)
fun addsv s v = mmap (fn x => x + s) v     (* scalar-vector addition *)
fun divv v1 v2 = sum Double (op /) v1 v2   (* element-wise vector division *)
fun signal (SIG (*:n*)) =
    catenate (scl (D 0.0)) SIG >>= (fn (c (*:n+1*)) =>
    diff c >>= (fn (v (*:n*)) => 
    divv v (addsv (D 0.01) SIG) >>= (fn tmp =>
    ret(maxsv (D ~50.0) (minsv (D 50.0) (prodsv (D 50.0) tmp))))))
```

In APL, the code is written

```apl
diff ← {1↓⍵−¯1⌽⍵}
signal ← {¯50⌈50⌊50×diff(0,⍵)÷0.01+⍵}
```

Here is the driver code for the program:

```sml
  val program =
    let val n = I 500
        val v = iota (I 2 * n)
        infix %
        val v = mmap (fn x => x % (I 200)) v
        val v = mmap i2d v
        val v = mmap (fn d => d / D 2.0) v
    in mem v >>= (fn v =>
       signal v >>= (fn v' =>
       red (ret o op +) (D 0.0) v'))
    end
```

The driver code first constructs an array of 1000 elements, processes
the elements of the array (by calling the `signal` function), and
finally, sums up the resulting array.

Here is the residual program generated (slightly modified):

```c
double kernel() {
  double[] n468 = alloc(1000*sizeof(double));
  for (int n478 = 0; n478 < 1000; n478++) {
    n468[n478] = (i2d((1+n478)%200)/2.0);
  }
  double n471 = 0.0;
  for (int n479 = 0; n479 < 1000; n479++) {
    n471 = (max(min((((((n479<1) ? 0.0 : n468[(-1+n479)])-n468[n479])/(n468[n479]+0.01))*50.0),50.0),-50.0)+n471);
  }
  return n471;
}
```

The first part of the generated program constructs a 1000-element
array of doubles. The second part of the generated program processes
each element of the array and sums up the results.

## Related work

There are lots of related work involving implementation of array languages.

 * [Open source APL interpreter in Javascript](http://ngn.github.com/apl/web/index.html). [Code](http://ngn.github.com/apl/docs/builtins.html).