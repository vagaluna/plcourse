(* This is a comment. This is our first program. *)

signature SS = 
sig
    type st 
    val add1 : int -> int
    val make_st : int * int -> st
end

structure Hehe :> SS = 
struct
    fun test s = s ^ "!"
    fun add1 x = x + 1
    type st = int * int
    fun make_st (x, y) = (x, y)
        
    (*structure Lala = 
    struct
        fun add2 x = add1 (add1 x)
    end

    fun test x = (Lala.add2 x) * 2*)
end


