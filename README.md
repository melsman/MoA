## MoA

Multi-dimentional array calculus in Standard ML based on the paper:

G. Hains et L. M. R. Mullin. An algebra of multidimensional arrays. Publication 783, DIRO, Departement 
d'Informatique et de Recherche Operationnelle, Universite de Montreal, 1991.

### Underlying vector implementations

The sources contains various basic vector library implementations that
are used as the basis for the MoA realization. The `vec/` folder
contains three implementations of the `VEC` signature:

  1. A reference vector implementation based on Standard ML lists.

  1. An implementation based on _pull arrays_, which consists of a
     number denoting the size of the array and a function for reading
     an element of the array, given an index. Pull arrays support map
     fusion and work well up-to a number of extensions, including drop
     and take functionality.

  1. An implementation based on both pull arrays and _push arrays_,
     which are continuation-based representations of arrays, which are
     dual to pull arrays in the sense that they support concatenation
     well, but drop and take operations not so well.

The `ilvec/` folder contains two different vector libraries that
support map fusion and generate residual (C-like) code. The second
implementation (`il2.mlb`) supports nesting of vectors and thus,
nested folds. As an example, lets first construct a matrix containing
the small multiplification table:

    val mat = tabulate (I 10) (fn i => tabulate (I 10) (fn j => i * j))
                                   
Here the `I` function lifts integers into the vector expression
language, on which all calculations are performed. The lifting allows
operations on vector expressions to be residualized into the C-like
target language. Now, let's write some code for summing up all the
values in the table:

    val m = foldr (fn (r, a) => foldr (ret o op +) a r) (I 0) mat

Here we see two nested folds, with the outer foldr folding over the
rows of the matrix with `I 0` as the base value for the
accumulator. The inner foldr adds all elements in a row to the outer
accumulator.

Here are the types of `foldr` and `tabulate`:

    val foldr    : ('a t * 'b t -> 'b t M) -> 'b t -> 'a v -> 'b t M
    val tabulate : INT -> (INT -> 'a t) -> 'a v

We see that `foldr` returns a monadic value, which we can "run" using
the `runM` function, which again generates a residual program
(slightly simplified):

    n49 = 0;
    for (int n52 = 0; n52 < 10; n52++) {
        n53 = n49;
        for (int n55 = 0; n55 < 10; n55++) {
            n53 += ((9-n52)*(9-n55));
        }
        n49 = n53;
    }

After executing the above code, the result is present in variable
`n49`.

