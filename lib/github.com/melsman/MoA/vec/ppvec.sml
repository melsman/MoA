(* Fusable vectors; pull arrays and push arrays.

   To allow for tk and dr operations on push arrays, the base
   iterators need to be general enough that they can adapt to the interval
 *)

structure PPvec :> VEC = struct

datatype dir = R | L
datatype 'a t = Pull of int * (int -> 'a)
              | Push of int * (dir * (int * 'a -> unit) -> unit)

fun fromList (l:'a list) :  'a t =
    let val a = Vector.fromList l
    in Pull (Vector.length a, fn i => (*Unsafe.*)Vector.sub(a,i))
    end

fun map f (Pull(n,g)) = Pull(n, f o g)
  | map f (Push(n,g)) = Push(n, fn (d,k) => g (d, fn(i,v) => k (i, f v)))

fun fmap a v = map (fn f => f v) a

fun iter d (n,e,f) =
    case d of
      L => let fun loop (i,a) = if i >= n then a else loop (i+1, f(i,a))
           in loop(0,e)
           end
    | R => let fun loop (i,a) = if i < 0 then a else loop(i-1, f(i,a))
           in loop(n-1,e)
           end

fun itr d ((x,y),f) =
    case d of
      L => let fun loop i = if i > y then () else (f i; loop (i+1))
           in loop x
           end
    | R => let fun loop i = if i < x then () else (f i; loop (i-1))
           in loop y
           end

fun fold d f e (Pull(n,g)) = iter d (n,e,fn (i,a) => f(g i,a))
  | fold d f e (Push(n,g)) = let val a = ref e
                             in g (d, fn (i,v) => a := f(v,!a)); !a
                             end
fun foldl x = fold L x
fun foldr x = fold R x

fun emp _ = raise Fail "impossible"
fun empty () = Pull(0, emp)

fun length (Pull(n,_)) = n
  | length (Push(n,_)) = n

fun memoize (Pull(n,f)) =
    let val v = Vector.tabulate(n,f)
    in Pull(n, fn i => (*Unsafe.*)Vector.sub(v,i))
    end
  | memoize (Push(n,f)) =
    let val a = ref NONE
        fun getA v = case !a of SOME arr => arr
                              | NONE => let val arr = Array.tabulate(n,fn _ => v)
                                        in a := SOME arr; arr
                                        end
    in f (L, fn (i,v) => Array.update(getA v,i,v));
       Pull(n, fn i => Array.sub(valOf(!a),i))
    end

fun tk n (a as Pull(m,g)) = if n >= m then a else Pull(n,g)
  | tk n (a as Push(m,g)) = if n >= m then a else tk n (memoize a)

fun dr n (Pull(m,g)) = if n >= m then empty() else Pull(m-n,fn i => g(i+n))
  | dr n (a as Push(m,g)) = if n >= m then empty() else dr n (memoize a)

fun tabulate n f = Pull(n,f)

fun list a = foldr (op ::) nil a

fun map2 f (Pull(n1,f1)) (Pull(n2,f2)) =
    if n1 <> n2 then raise Fail "map2 applied to vectors of different lengths"
    else Pull(n1,fn i => f(f1 i)(f2 i))
  | map2 f (a1 as Push _) a2 = map2 f (memoize a1) a2
  | map2 f a1 (a2 as Push _) = map2 f a1 (memoize a2)

fun single x = fromList [x]

fun eq beq (a1,a2) =
    length a1 = length a2 andalso
    (foldl(fn (x,a) => x andalso a) true 
	  (map2 (fn x => fn y => beq(x,y)) a1 a2))
    
fun push (Pull(n,f)) = (n,fn (d,k) => itr d ((0,n-1),fn i => k(i, f i)))
  | push (Push a) = a

fun min a b = if a < b then a else b
fun max a b = if a > b then a else b
fun pos x = max 0 x

(* process a list of elements each containing a series of items *)
fun flat d k ((m,f),(n,j)) = (* n: remaining elements; j already read elements *)
    let val k' =
            case d of
              L => (fn (i,v) => k(i+j,v))
            | R => (fn (i,v) => k(i+n-m,v))
    in f(d,k'); (n-m,j+m)
    end

fun flatten v =
    let val nfs = map push v
        val n = fold L (op +) 0 (map #1 nfs)
        fun f (d,k) = (fold d (flat d k) (n,0) nfs; ())
    in Push(n,f)
    end

fun listfold R = List.foldr
  | listfold L = List.foldl

fun flattenL v =
    let val nfs = List.map push v
        val n = listfold L (op +) 0 (List.map #1 nfs)
        fun f (d,k) = (listfold d (flat d k) (n,0) nfs; ())
    in Push(n,f)
    end

fun concat x y = flattenL [x,y]

fun sub (Pull(n,f),i) =
    if i > n-1 orelse i < 0 then raise Subscript
    else f i
  | sub (Push(n,f),i) = raise Fail "PPvec.sub.Push.unimplemented"

fun rev (Pull(n,f)) = Pull(n,fn i => f(n-i-1))
  | rev (Push(n,f)) = Push(n,fn (d,k) => f(d,(fn (i,v) => k(n-i-1,v))))
end
