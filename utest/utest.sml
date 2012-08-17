structure UTest :> UTEST = struct

  val counts = {ok=ref 0, wrong=ref 0, exn=ref 0}
  fun incr l =
      let val r = l counts
      in r := !r + 1
      end
  fun ok() = (incr #ok; "OK")
  fun wrong s = (incr #wrong; if s = "" then "WRONG" else "WRONG - " ^ s)
  fun exn() = (incr #exn; "EXN")
  fun check f = 
      (case f () of SOME s => wrong s
		  | NONE => ok())
      handle e => (exn() ^ General.exnMessage e)

  fun tst0 s s' = print (s ^ "    \t" ^ s' ^ "\n")
  fun tstopt s f = tst0 s (check f)
  fun tst s f = tst0 s (check (fn x => if f() then NONE else SOME""))
  fun all s f xs =
      tst s (fn() => List.all f xs)
      
  val data : (string*string) option ref = ref NONE
  fun start f s =
      (data := SOME (f,s);
       #ok counts := 0;
       #wrong counts := 0;
       #exn counts := 0;
       print ("[File " ^ f ^ ": Testing " ^ s ^ "...]\n"))

  fun finish () =
      let val ok = ! (#ok counts)
          val wrong = ! (#wrong counts)
          val exn =  ! (#exn counts)
      in
        case !data of
          NONE => print "[Test not properly started]\n"
        | SOME (f,s) =>
          (print ("[Finished testing file " ^ f ^ " - " ^ s ^ "]\n");
           if wrong = 0 andalso exn = 0 then
             print ("[Successfully ran all " ^ Int.toString ok ^ " tests]\n")
           else
             print ("[Failure during tests - ok: " ^ Int.toString ok ^ ", wrong: " ^ Int.toString wrong ^ ", exn: " ^ Int.toString exn ^ "]\n")
          )
      end
end
