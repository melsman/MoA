functor Moa(A: VEC) :> MOA = struct

  type 'a t = int list * 'a A.t

  fun NormalForm xs =
      let fun norm (0::xs,acc) = [0]
            | norm (1::xs,acc) = norm (xs,acc)
            | norm (x::xs,acc) = norm (xs,x::acc)
            | norm ([],acc) = rev acc
      in norm (xs,[])
      end

  fun vec0 c = (NormalForm [A.length c],c)
  fun vec c = vec0 (A.fromList c)
  fun scl v = ([],A.single v)
  fun zilde () = ([0],A.empty())
  fun iota n = vec0 (A.tabulate n (fn x => x+1))
  fun shape (f,_) = f
  fun snd (_,c) = c
  fun product xs =
      let fun prod ([],acc) = acc
            | prod (x::xs,acc) = prod(xs, x*acc)
      in prod (xs,1)
      end
  fun siz (f,_) = product f
  fun rep f = NormalForm f
  fun dim (f,_) = List.length (rep f)

  (* Restructuring *)
  fun rav (_,c) = vec0 c
  fun reshape f a = if product f = siz a then (f,snd a) else zilde()

  fun dr 0 l = l
    | dr n [] = []
    | dr n (x::l) = dr (n-1) l

  fun tk 0 l = []
    | tk n [] = []
    | tk n (x::l) = x :: tk (n-1) l

  fun index0 i a =
      let val t = dr 1 (shape a)
          val p = product t
      in (reshape t o vec0 o A.tk p o A.dr (i * p) o snd) a
      end

  fun index [] a = a
    | index (i::is) a = index is (index0 i a)

  fun map g (f,cs) = (f, A.map g cs)

  fun fmap (f,c) v = (f, A.fmap c v)

  fun reduce g = foldl (fn (x,a) => g a x)
  fun areduce g = A.foldl (fn (x,a) => g a x)

  fun red g e (f,c) = areduce g e c

  fun pre [] = []
    | pre (t::l) = [t] :: List.map (fn l => t::l) (pre l)

  fun scan0 g e = List.map (reduce g e) o pre

  fun scan g e (f,c) = (f,A.fromList(scan0 g e (A.list c)))

  fun out g a b =
      let val f = shape a @ shape b
          val c = (A.flatten o A.fmap (A.map (A.map o g) (snd a))) (snd b)
      in (NormalForm f, c)
      end

  fun sum g a b =
      let val sha = shape a
	  val shb = shape b
      in if sha = shb then
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
      in reshape (s (shape a) (shape b)) (vec0 (A.concat (snd a) (snd b)))
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

  fun eq beq (a,b) = shape a = shape b andalso A.eq beq (snd a, snd b)
end

structure Moa = Moa(Fvec)
(*structure Moa = Moa(ListVec)*)
