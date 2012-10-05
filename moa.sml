signature SHAPE = sig
  type t
  val fromList : int list -> t
  val toList   : t -> int list
  val concat   : t * t -> t
  val single   : int -> t
  val empty    : t
  val product  : t -> int
  val length   : t -> int
  val dr       : int -> t -> t
  val tk       : int -> t -> t
  val eq       : t * t -> bool
end

structure Shape :> SHAPE = struct

  (* We maintain the below invariants on shape vectors. This approach
     is a correct implementation strategy according to Definition 3 in
     the paper.
  *)

  type t = int list  (* invariants:
                            A. if t contains 0 then t = [0]
                            B. t does not contain 1's
                      *)
  fun NormalForm xs =
      let fun norm (0::xs,acc) = [0]
            | norm (1::xs,acc) = norm (xs,acc)
            | norm (x::xs,acc) = norm (xs,x::acc)
            | norm ([],acc) = rev acc
      in norm (xs,[])
      end
  val fromList = NormalForm
  val toList = fn x => x
  fun concat (a, b) =
      case (a, b) of ([0],_) => a
                   | (_,[0]) => b
                   | _ => a @ b
  fun single 1 = []
    | single n = [n]
  val empty = []
  fun product xs = List.foldl (op *) 1 xs
  val length = List.length
  fun dr 0 l = l
    | dr n [] = []
    | dr n (x::l) = dr (n-1) l
  fun tk 0 l = []
    | tk n [] = []
    | tk n (x::l) = x :: tk (n-1) l
  val eq = op =
end

functor Moa(A: VEC) :> MOA = struct

  type 'a t = Shape.t * 'a A.t

  fun vec0 c = (Shape.single (A.length c), c)
  fun vec c = vec0 (A.fromList c)
  fun scl v = (Shape.empty, A.single v)
  fun zilde () = (Shape.single 0, A.empty())
  fun iota n = vec0 (A.tabulate n (fn x => x+1))
  fun shape0 (f,_) = f
  fun shape a = Shape.toList (shape0 a)
  fun snd (_,c) = c
  fun siz (f,c) = Shape.product f   (* or A.length c *)
  fun dim (f,_) = Shape.length f

  (* Restructuring *)
  fun rav (_,c) = vec0 c
  fun reshape0 f a = if Shape.product f = siz a then (f,snd a) else zilde()
  fun reshape f a = reshape0 (Shape.fromList f) a
  fun index0 i a =
      let val t = Shape.dr 1 (shape0 a)
          val p = Shape.product t
      in (reshape0 t o vec0 o A.tk p o A.dr (i * p) o snd) a
      end

  fun index [] a = a
    | index (i::is) a = index is (index0 i a)

  fun map g (f,cs) = (f, A.map g cs)

  fun fmap (f,c) v = (f, A.fmap c v)

  fun red0 g = A.foldl (fn (x,a) => g a x)

  fun red g e (f,c) = red0 g e c

  fun pre a =
      if A.length a = 0 then A.empty()
      else A.concat (pre (A.tk (A.length a - 1) a)) (A.single a)

  fun scan0 g e = A.map (red0 g e) o pre

  fun scan g e (f,c) = (f, scan0 g e c)

  fun out g a b =
      let val c = (A.flatten o A.fmap (A.map (A.map o g) (snd a))) (snd b)
      in (Shape.concat(shape0 a, shape0 b), c)
      end

  fun sum g a b =
      let val sha = shape0 a
	  val shb = shape0 b
      in if Shape.eq(sha,shb) then
	     (sha, A.map2 g (snd a) (snd b))
	 else zilde()
      end

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
              let val s1 = Shape.toList s1
                  val s2 = Shape.toList s2
              in Shape.fromList(s s1 s2)
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
end

(*structure Moa = Moa(Fvec)*)
structure Moa = Moa(PPvec)
(*structure Moa = Moa(ListVec)*)
