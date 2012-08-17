(* Fusable vectors *)

structure Fvec :> VEC = struct

type 'a t = int * (int -> 'a)

fun fromList (l:'a list) :  'a t =
    let val a = Vector.fromList l
    in (Vector.length a, fn i => Unsafe.Vector.sub(a,i))
    end

fun map f (n,g) = (n, f o g)

fun fmap a v = map (fn f => f v) a

fun iter (n,e,f) =
    let fun loop (i,a) =
	    if i >= n then a
	    else loop (i+1, f(i,a))
    in loop(0,e)
    end

fun iter' (n,e,f) =
    let fun loop (i,a) =
	    if i <= 0 then a
	    else let val i2 = i-1
		 in loop (i2, f(i2,a))
		 end
    in loop(n,e)
    end

fun foldl f e (n,g) = iter(n,e, fn (i,a) => f(g i,a))
fun foldr f e (n,g) = iter'(n,e, fn (i,a) => f(g i,a))

fun emp _ = raise Fail "impossible"
fun empty () = (0, emp)

fun tk n (a as (m,g)) = 
    if n >= m then a
    else (n,g)

fun dr n (m,g) =
    if n >= m then empty()
    else (m-n,fn i => g(i+n))

fun tabulate n f = (n,f)

fun list a = foldr (op ::) nil a
	
fun length (n,_) = n

fun map2 f (n1,f1) (n2,f2) =
    if n1 <> n2 then raise Fail "map2 applied to vectors of different lengths"
    else (n1,fn i => f(f1 i)(f2 i))

fun single x = fromList [x]

fun eq beq (a1,a2) =
    length a1 = length a2 andalso
    (foldl(fn (x,a) => x andalso a) true 
	  (map2 (fn x => fn y => beq(x,y)) a1 a2))

fun concat (n1,f1) (n2,f2) =
    (n1+n2, fn i => if i < n1 then f1 i else f2 (i-n1))

fun flatten v =
    let val len = foldl (fn (a,l) => length a + l) 0 v
	val f = foldr (fn ((n,f),g) =>
		       fn i => if i < n then f i else g (i-n)) emp v
    in (len,f)
    end

fun sub ((n,f),i) =
    if i > n-1 orelse i < 0 then raise Subscript
    else f i

fun memoize (n,f) =
    let val v = Vector.tabulate(n,f)
    in (n, fn i => Unsafe.Vector.sub(v,i))
    end
end
