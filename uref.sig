(* uref.sig
 *
 * Interface to UnionFind package.
 *
 * Author:
 *    Fritz Henglein
 *    DIKU, University of Copenhagen
 *    henglein@diku.dk
 *
 * DESCRIPTION
 *
 * Union/Find data type with ref-like interface.  A Union/Find
 * structure consists of a type constructor 'a uref with operations
 * for making an element of 'a uref (uref), getting the contents of an
 * element (!!), checking for equality of two elements (equal), and
 * for joining two elements (unify).  uref is analogous to ref as
 * expressed in the following table:
 *
 * -------------------------------------------------------------------
 * type                  'a ref                'a uref
 * -------------------------------------------------------------------
 * introduction          ref                   uref
 * elimination           !                     !!
 * equality              =                     equal
 * updating              :=                    ::=
 * unioning                                    unify
 * -------------------------------------------------------------------
 *
 * The main difference between 'a ref and 'a uref is in the union
 * operation.  Without union 'a ref and 'a uref can be used
 * interchangebly.  An assignment to a reference changes only the
 * contents of the reference, but not the reference itself.  In
 * particular, any two pointers that were different (in the sense of
 * the equality predicate = returning false) before an assignment will
 * still be so.  Their contents may or may not be equal after the
 * assignment, though.  In contrast, applying the union operation
 * (unify) to two uref elements makes the two elements themselves
 * equal (in the sense of the predicate equal returning true).  As a
 * consequence their contents will also be identical. The contents is
 * determined by a binary function parameter.
 *)

signature UREF = sig
  type 'a uref
  val uref   : 'a -> 'a uref                   
  val equal  : 'a uref * 'a uref -> bool
  val !!     : 'a uref -> 'a
  val ::=    : 'a uref * 'a -> unit
  val unify  : ('a * 'a -> 'a) -> 'a uref * 'a uref -> unit
end

(**
  ['a uref] type of uref-elements with contents of type 'a.

  [uref x] creates a new element with contents x.

  [equal (e, e')] returns true if and only if e and e' are either made
  by the same call to uref or if they have been unioned (see below).

  [!!e] returns the contents of e. Note: if 'a is an equality type
  then !!(uref x) = x, and equal(uref (!!x), x) = false.

  [e ::= x] updates the contents of e to be x.

  [unify f (e, e')] makes e and e' equal; if v and v' are the contents
  of e and e', respectively, before unioning them, then the contents
  of the unioned element is f(v,v').
*)  
          
