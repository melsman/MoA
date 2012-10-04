## MoA

Multi-dimentional array calculus in Standard ML based on the paper:

G. Hains et L. M. R. Mullin. An algebra of multidimensional arrays. Publication 783, DIRO, Departement 
d'Informatique et de Recherche Operationnelle, Universite de Montreal, 1991.

### Underlying vector implementations

The sources contains various basic vector library implementations that
are used as the basis for the MoA realization. The `vec/` folder
contains three implementations of the `VEC` signature:

  1. A reference vector implementation based on Standard ML lists.

  1. An implementation based on _pull arrays_, which are basically
     arrays comprised of the size of the array together with a
     function for reading an element of the array, given an
     index. Pull arrays support map fusion and work well up-to a
     number of extensions, including drop and take functionality.

  1. An implementation based on both pull arrays and _push arrays_,
     which are continuation-based represenations of arrays, which are
     dual to pull arrays in the sense that they support concatenation
     well, but drop and take constructs not so well.

The `ilvec/` folder contains two different vector libraries that
support map fusion and generates residual (C-like) code. The second
implementation (`il2.mlb`) supports nesting of vectors and thus,
nested folds...

