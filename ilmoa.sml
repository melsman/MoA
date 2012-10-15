
structure ILmoa :> ILMOA = struct

fun die s = raise Fail ("ILmoa." ^ s)

structure P = Program
structure Term = struct
  datatype t0 = E of P.e
              | V of P.e * (P.e -> t0)
              | MV of t0 * t0
  fun unE (E e) = SOME e
    | unE _ = NONE
  fun unV (V v) = SOME v
    | unV _ = NONE
  fun unMV (MV p) = SOME p
    | unMV _ = NONE
end

structure ILvec = ILvec(Term)
open ILvec
open Term

structure Shape : sig
  type t = t0
  val fromVec  : INT v -> t
  val toVec    : t -> Int v
  val concat   : t -> t -> t
  val single   : INT -> t
  val empty    : t
  val product  : t -> INT M
  val length   : t -> INT
  val dr       : INT -> t -> t
  val tk       : INT -> t -> t
  val eq       : t -> t -> BOOL M
end = struct
  (* We maintain the below invariants on shape vectors. This approach
     is a correct implementation strategy according to Definition 3 in
     the paper.
  *)
  type t = t0
  val toVec    = fn x => x
  val fromVec  = shapify
  val concat   = fn x => fn y => shapify(concat x y)
  val single   = single
  val empty    = empty Int
  val product  : t -> INT M = foldl (ret o op *) (I 1)
  val length   : t -> INT = length
  val dr       : INT -> t -> t = dr
  val tk       : INT -> t -> t = tk
  val eq       : t -> t -> BOOL M = eq (op ==)
end

type 'a MVec = unit
type 'a m = 'a MVec t

fun vec00 n c =
    let fun default() = MV(Shape.single n, c)
    in case unE n of
         SOME n =>
         (case P.unI n of
            SOME 1 => MV(Shape.empty, c)
          | _ => default())
       | NONE => default()
    end

fun vec0 c = vec00 (length c) c
fun vec c = vec0 (Shape.fromVec c)
fun scl v = MV(Shape.empty, single v)
fun zilde ty = MV(Shape.single (I 0), empty ty)
fun iota n = vec00 n (tabulate n (fn x => x + (I 1)))
fun shape0 t =
    case unMV t of
      SOME (f,_) => f
    | NONE => die "shape0: expecting moa array"
fun shape a = Shape.toVec (shape0 a)
fun snd t = 
    case unMV t of
      SOME (_,c) => c
    | NONE => die "snd: expecting moa array"
fun siz t =
    case unMV t of
      SOME (f,c) => (*Shape.product f*) length c
    | NONE => die "siz: expecting moa array"
fun dim t = 
    case unMV t of
      SOME (f,_) => Shape.length f
    | NONE => die "dim: expecting moa array"
              
(* Restructuring *)
fun rav t = 
    case unMV t of
      SOME(_,c) => vec0 c
    | NONE => die "rav: expecting moa array"

fun mif (x,a1,a2) =
    case (unMV a1, unMV a2) of
      (SOME (f1,v1), SOME (f2,v2)) =>
      MV(If(x,f1,f2), If(x,v1,v2))
    | _ => die "mif: expecting moa arrays"

fun zildeOf a =
    case unMV a of
      SOME(f,v) => MV(Shape.single (I 0), emptyOf v)
    | NONE => die "zildeOf: expecting moa array"

infix >>= ==
fun reshape0 (f: Int v) (a: 'a m) : 'a m M =
    Shape.product f >>= (fn p =>
      ret(mif(p == siz a, MV(f,snd a), zildeOf a)))
    
fun reshape f a = reshape0 (Shape.fromVec f) a
                  
fun index0 (i: INT) (a : 'a m) : 'a m M =
      let val t = Shape.dr (I 1) (shape0 a)
          val v = snd a
      in Shape.product t >>=
                       (fn p =>
                           let val v1 = dr (i * p) v
                               val v2 = tk p v1
                               val mv = vec0 v2
                           in reshape0 t mv
                           end)
      end

fun index (v: INT v) (mv : 'a m) : 'a m M =
    foldl (fn (i,a) => index0 i a) mv v

fun mmap g t =
    case unMV t of
      SOME (f,cs) => MV(f, ILvec.map g cs)
    | NONE => die "map: expecting moa array"
      
(*        
fun fmap t v =
    case unMV t of
      SOME (f,c) => MV(f, ILvec.fmap c v)
    | NONE => die "fmap: expecting moa array"
*)

fun red g e t =
    case unMV t of
      SOME (f,c) => foldl g e c
    | NONE => die "red: expecting moa array"

fun meq f t1 t2 =
    case (unMV t1, unMV t2) of
      (SOME (f1,c1), SOME (f2,c2)) =>
      Shape.eq f1 f2 >>= (fn shape_eq => 
      eq f c1 c2 >>= (fn content_eq =>
      ret(If(shape_eq,content_eq,B false))))
    | _ => die "meq: expecting moa arrays"

fun out (ty:'c T) (g: 'a t * 'b t -> 'c t) (xs: 'a m) (ys: 'b m) : 'c m M =
    let val res : 'c Vec v =
            map (fn x => 
                    map (fn y => g(x,y)) (snd ys))
                (snd xs)
        val sh = Shape.concat(shape0 xs)(shape0 ys)
    in flatten ty res >>= (fn c : 'c v =>
       ret(MV(sh,c)))
    end

fun sum ty g a b =
    let val sha = shape0 a
	val shb = shape0 b
        val mv = MV(sha,map2 g (snd a) (snd b))
    in Shape.eq sha shb >>= (fn shapeeq =>
       ret(mif(shapeeq, mv, zilde ty)))
    end

fun pre (a: 'a v) : 'a Vec v =
    let val n = length a
        val iotan = tabulate n (fn x => x + I 1)
    in map (fn i => tk i a) iotan
    end

fun mapm emp f xs = 
    foldl (fn (x,a) => f x >>= (fn y => ret(concat a (single y)))) emp xs

fun scan0 g e a = 
    mapm (emptyOf e) (foldl (ret o g) e) (pre a)

fun scan g e t =
    case unMV t of
      SOME (f,c) => scan0 g e c >>= (fn c' => ret(MV(f,c')))
    | NONE => die "scan: expecting moa array"

(*
  fun stk a b =
      let fun s [0] ys = ys
	    | s xs [0] = xs
	    | s xs ys =
	      let fun try1 (x::xs) =
		      if x <> 0 andalso xs = ys then x+1::xs
		      else try2 ys
		    | try1 nil = try2 ys
		  and try2 (y::ys) =
		      if y <> 0 andalso ys = xs then y+1::ys
		      else [0]
		    | try2 nil = [0]
	      in
		  if xs = ys then 2 :: xs
		  else try1 xs
	      end
          fun s' s1 s2 =
              let val s1 = Shape.toVec s1
                  val s2 = Shape.toVec s2
              in Shape.fromVec(s s1 s2)
              end
      in reshape0 (s' (shape0 a) (shape0 b)) (vec0 (A.concat (snd a) (snd b)))
      end

  fun pp f a =
      let fun arr s = "[" ^ s ^ "]"
	  fun pr [] (v::vs) = (f v, vs)
            | pr [] vs = ("",vs)
	    | pr (n::sh) vs =
	      let fun loop n vs = if n <= 0 then ("",vs)
				  else if n = 1 then pr sh vs
				  else let val (s,vs) = pr sh vs
					   val (rest,vs) = loop (n-1) vs
				       in (s ^ "," ^ rest, vs)
				       end
		  val (s, vs) = loop n vs
	      in (arr s, vs)
	      end
      in #1 (pr (shape a) (A.list(#2 a)))
      end 

  fun eq beq (a,b) = Shape.eq(shape0 a, shape0 b) andalso A.eq beq (snd a, snd b)
*)
end
