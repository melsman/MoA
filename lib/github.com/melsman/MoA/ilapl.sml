
structure ILapl :> ILAPL = struct

fun die s = raise Fail ("ILapl." ^ s)

structure P = Program
structure Term = struct
  type 'a M = 'a * (P.ss -> P.ss)
  datatype t0 = E of P.e
              | V of IL.Type * P.e * (P.e -> t0 M)
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

val addi = op +
val subi = op -
val muli = op *
val divi = op /
val lti = op <
val leqi = op <=
val eqi = op ==
val maxi = max
val mini = min
val negi = ~

val addd = op +
val subd = op -
val muld = op *
val divd = op /
val ltd = op <
val leqd = op <=
val eqd = op ==
val maxd = max
val mind = min
val negd = ~

infix >>= == %

structure Shape : sig
  type t = t0
  val concat   : t -> t -> t
  val single   : INT -> t
  val singlez  : t
  val empty    : t
  val product  : t -> INT M
  val length   : t -> INT
  val dr       : INT -> t -> t
  val tk       : INT -> t -> t
  val eq       : t -> t -> BOOL M
end = struct
  type t = t0
  val concat   = concat
  val empty    = empty Int
  val singlez  = single Int (I 0)
  val single   = single Int
  val product  : t -> INT M = foldl (ret o op *) (I 1)
  val length   : t -> INT = length
  val dr       : INT -> t -> t = dr
  val tk       : INT -> t -> t = tk
  val eq       : t -> t -> BOOL M = eq (op ==)
end

type 'a MVec = unit
type 'a m = 'a MVec t

(* invariant: MV(s,d) ==> product s = length d *) 

fun vec c = MV(Shape.single (length c), c)
fun scl ty v = MV(Shape.empty, single ty v)
fun first _ = raise Fail "ilapl.first unimplemented"
fun zilde ty = MV(Shape.singlez, empty ty)
fun iota n = vec (tabulate Int n (fn x => ret(x + (I 1))))
fun iota' _ = raise Fail "ilapl.iota' not implemented"
fun shape0 t =
    case unMV t of
      SOME (f,_) => f
    | NONE => die "shape0: expecting apl array"
fun shape a = shape0 a
fun snd t = 
    case unMV t of
      SOME (_,c) => c
    | NONE => die "snd: expecting apl array"
fun siz t =
    case unMV t of
      SOME (f,c) => length c
    | NONE => die "siz: expecting apl array"
fun dim t = 
    case unMV t of
      SOME (f,_) => Shape.length f
    | NONE => die "dim: expecting apl array"
              
(* Restructuring *)
fun rav t = 
    case unMV t of
      SOME(_,c) => vec c
    | NONE => die "rav: expecting apl array"

val rav0 = snd

fun mif (x,a1,a2) =
    case (unMV a1, unMV a2) of
      (SOME (f1,v1), SOME (f2,v2)) =>
      MV(If(x,f1,f2), If(x,v1,v2))
    | _ => die "mif: expecting apl arrays"

fun zildeOf a =
    case unMV a of
      SOME(f,v) => MV(Shape.singlez, emptyOf v)
    | NONE => die "zildeOf: expecting apl array"

fun each _ ty g t =
    case unMV t of
      SOME (f,cs) => MV(f, ILvec.map ty g cs)
    | NONE => die "each: expecting apl array"
      
fun red _ _ g e t =
    case unMV t of
      SOME (f,c) => foldl g e c
    | NONE => die "red: expecting apl array"

fun meq _ f t1 t2 =
    case (unMV t1, unMV t2) of
      (SOME (f1,c1), SOME (f2,c2)) =>
      Shape.eq f1 f2 >>= (fn shape_eq => 
      eq f c1 c2 >>= (fn content_eq =>
      ret(If(shape_eq,content_eq,B false))))
    | _ => die "meq: expecting apl arrays"

(*
fun out (ty:'c T) (g: 'a t * 'b t -> 'c t) (xs: 'a m) (ys: 'b m) : 'c m M =
    let val res : 'c Vec v =
            map (fn x => 
                    map (fn y => g(x,y)) (snd ys))
                (snd xs)
        val sh = Shape.concat(shape0 xs)(shape0 ys)
    in flatten ty res >>= (fn c : 'c v =>
       ret(MV(sh,c)))
    end
*)
fun sum _ _ ty g a b =
    let val sha = shape0 a
	val shb = shape0 b
        val mv = MV(sha,map2 ty g (snd a) (snd b))
    in Shape.eq sha shb >>= (fn shapeeq =>
       ret(mif(shapeeq, mv, zilde ty)))
    end

fun pre (a: 'a v) : 'a Vec v =
    let val n = length a
        val iotan = tabulate Int n (fn x => ret(x + I 1))
    in map (Vec(tyOfV a)) (fn i => ret(tk i a)) iotan
    end

fun mapm emp f xs = 
    foldl (fn (x,a) => f x >>= (fn y => ret(concat a (single (tyOfV a) y)))) emp xs

fun scan0 g e a = 
    mapm (emptyOf e) (foldl (ret o g) e) (pre a)

fun scan _ _ g e t =
    case unMV t of
      SOME (f,c) => scan0 g e c >>= (fn c' => ret(MV(f,c')))
    | NONE => die "scan: expecting moa array"

fun pad1 s = If(length s == I 0, single Int (I 1), s) 
fun catenate_first (t1 : 'a m) (t2: 'a m) : 'a m M =
    case (unMV t1, unMV t2) of
      (SOME (s1,d1), SOME (s2,d2)) =>
      let val s1 = pad1 s1
          val s2 = pad1 s2                   
          val s1' = Shape.dr (I 1) s1
          val s2' = Shape.dr (I 1) s2
          val v1 = Shape.tk (I 1) s1
          val v2 = Shape.tk (I 1) s2
          val x = map2 Int (ret o op +) v1 v2
          val mv = MV(Shape.concat x s1',
                      concat d1 d2)
      in ret mv (*Shape.eq s1' s2' >>= (fn shapeeq =>
          ret(mif(shapeeq,mv,zildeOf t1)))*)
      end
    | _ => die "catenate_first: expecting moa arrays"

fun take0 n (t : 'a m) : 'a m = vec(tk n (snd t))
fun drop0 n (t : 'a m) : 'a m = vec(dr n (snd t))

fun mem t =
    case unMV t of
      SOME(f,d) => 
      memoize d >>= (fn d => ret (MV(f,d)))
    | NONE => die "mem: expecting apl array"

val take : INT -> 'a m -> 'a m = fn n => fn t =>
   mif(n < I 0, drop0 (siz t + n) t, take0 n t)
and drop : INT -> 'a m -> 'a m = fn n => fn t =>
   mif(n < I 0, take0 (siz t + n) t, drop0 n t)

fun rotate n t =
    case unMV t of
      SOME(f,d) =>
      let val sz = length d
      in mif(n < I 0,
             MV(f,concat (dr (sz + n) d) (tk (sz + n) d)),
             MV(f,concat (dr n d) (tk n d)))
      end
    | NONE => die "rotate: expecting apl array"

fun reshape (f: Int v) (a: 'a m) : 'a m M =
    Shape.product f >>= (fn p =>
    ret(MV(f,extend p (snd a))))

fun transpose t =
    case unMV t of
      SOME (s,d) => MV(rev s, trans s d)
    | NONE => die "APL.trans: expecting array"

fun transpose2 _ = raise Fail "ilapl.transpose2: not implemented"
fun reverse _ = raise Fail "ilapl.reverse: not implemented"

fun catenate t1 t2 = catenate_first (transpose t1) (transpose t2) >>= (ret o transpose)

fun eOfT t =
    case unE t of
        SOME e => e
      | NONE => die "APL.eOfT: expecting E"

fun reduce _ f e t scalar vector =
    case unMV t of
      SOME (s,d) => 
      let val r = length s
      in case unE r of
           SOME r =>
           (case P.unI r of
              SOME 1 => foldl f e d >>= (ret o scalar)
            | SOME 2 =>  (* matrix: M x N *)
              let 
              in sub_unsafe s (I 0) >>= (fn M =>
                 sub_unsafe s (I 1) >>= (fn N =>
                 ret (MV(Shape.single M,
                    V(tyOfV d, eOfT M,
                      fn i => foldl f e (tk N (dr (E i * N) d))))))) >>= (ret o vector)
              end
            | SOME n => die ("reduce: rank " ^ Int.toString n ^ " not supported")      
            | NONE => die "reduce: unknown rank not supported")
         | _ => die "reduce: expecting length to return an expression"
      end
    | _ => die "reduce: expecting vector"

fun build2 ty M1 M2 f =
    case unE (M1*M2) of
        SOME N => ret (V(ty, N, fn i => f (E i / M2) (E i % M2)))
      | NONE => die "build2.expecting expression"
           
fun prod _ f g e m1 m2 scalar array =
    let val m2T = transpose m2
    in case (unMV m1, unMV m2T) of
         (SOME (s1,d1), SOME(s2,d2)) => 
         let val r1 = length s1
             val r2 = length s2
             val ty = tyOfV d1
         in case (unE r1, unE r2) of
              (SOME r1, SOME r2) =>
              (case (P.unI r1, P.unI r2) of
                 (SOME 1, SOME 1) => 
                 foldl f e (map2 ty g d1 d2) >>= (fn v => ret(scalar v))
               | (SOME 2, SOME 2) =>  (* matrix: M x N *)
                 sub_unsafe s1 (I 0) >>= (fn M1 =>
                 sub_unsafe s2 (I 0) >>= (fn M2 =>
                 sub_unsafe s1 (I 1) >>= (fn N1 =>
                 sub_unsafe s2 (I 1) >>= (fn N2 =>                 
                 let val s = fromList Int [M1,M2] 
                 (* memo: check N1 = N2 *)
                 in memoize d1 >>= (fn d1 =>
                    memoize d2 >>= (fn d2 =>
                    build2 ty M1 M2 (fn x => fn y =>
                                        let val v1 = tk N1 (dr (x*N1) d1)
                                            val v2 = tk N2 (dr (y*N2) d2)
                                        in foldl f e (map2 ty g v1 v2)
                                        end) >>= (fn a => ret(array (MV(s,a))))))
                 end))))
               | (SOME n, SOME n') => die ("prod: rank " ^ Int.toString n ^ ", " ^ 
                                           Int.toString n' ^ " not supported")      
               | _ => die "prod: unknown ranks not supported")
            | _ => die "prod: expecting length to return an expression"
         end
       | _ => die "prod: expecting arrays"
    end
    
fun lett _ = ret
fun letm _ = ret
fun letm_asgn _ = mem

end
