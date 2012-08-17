structure ListVec : VEC = struct
  type 'a t = 'a list

  fun eq beq (nil,nil) = true
    | eq beq (x::xs, y::ys) = beq(x, y) andalso eq beq (xs,ys)
    | eq beq _ = false

  val empty = fn () => []
  val single = fn x => [x]

  fun dr 0 l = l
    | dr n [] = []
    | dr n (x::l) = dr (n-1) l

  fun tk 0 l = []
    | tk n [] = []
    | tk n (x::l) = x :: tk (n-1) l
  
  val list = fn x => x
  val fromList = fn x => x
  open List
  val concat = fn x => fn y => x @ y
  fun map2 f _ nil = nil
    | map2 f nil _ = nil
    | map2 f (x::xs) (y::ys) = f x y :: map2 f xs ys
  fun fmap gs x =
      let fun loop [] = []
	    | loop (g::gs) = g x :: loop gs
      in loop gs
      end
  fun tabulate n f =
      let fun gen x = if x >= n then []
		      else f x :: gen (x+1)
      in gen 0
      end
  fun flatten nil = nil
    | flatten (x::xs) = x @ flatten xs

  val sub = List.nth

  val memoize = fn x => x
end
