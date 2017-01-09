(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
         | Variable of string
         | UnitP
         | ConstP of int
         | TupleP of pattern list
         | ConstructorP of string * pattern

datatype valu = Const of int
          | Unit
          | Tuple of valu list
          | Constructor of string * valu

fun g f1 f2 p =
    let 
    val r = g f1 f2 
    in
    case p of
        Wildcard          => f1 ()
      | Variable x        => f2 x
      | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
      | ConstructorP(_,p) => r p
      | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
         | UnitT
         | IntT
         | TupleT of typ list
         | Datatype of string

(**** you can put all your code here ****)

(* Probelm 1 *)
fun only_capitals strings =
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) strings


(* Problem 2 *)
fun longest_string1 strings =
    foldl 
        (fn (s, longest) => 
            let 
                val len = String.size s
                val max_len = String.size longest
            in if len > max_len then s else longest
            end)
        ""
        strings

(* Problem 3 *)
fun longest_string2 strings =
    foldl 
        (fn (s, longest) => 
            let 
                val len = String.size s
                val max_len = String.size longest
            in if len >= max_len then s else longest
            end)
        ""
        strings


(* Problem 4 *)
fun longest_string_helper comparator strings =
    foldl
        (fn (s, longest) =>
            let
                val len = String.size s
                val max_len = String.size longest
            in
                if comparator(len, max_len) then s else longest
            end)
        ""
        strings

val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)


(* Problem 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* Problem 6 *)
val rev_string = String.implode o rev o String.explode


(* Problem 7 *)
fun first_answer test xs =
    case xs of
        [] => raise NoAnswer
        | x::xs' => 
            case test x of
                SOME v => v
                | NONE => first_answer test xs'


(* Problem 8 *)
fun all_answers test xs =
    let
        fun helper(remain, acc) = 
            case remain of
                [] => SOME acc
                | x::xs' => 
                    case test x of
                        NONE => NONE
                        | SOME lst => helper(xs', acc @ lst)
    in
        helper(xs, [])
    end



(* Problem 9 *)

(* part a *)
val count_wildcards = g (fn () => 1) (fn x => 0)

(* part b *)
val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x)

(* part c *)
fun count_some_var(s, p) = 
    g (fn () => 0) (fn x => if x = s then 1 else 0) p



(* Problem 10 *)
fun check_pat p =
    let
        fun all_vars p = 
            case p of
                Variable s => [s]
                | TupleP ps => foldl (fn (pt, vars) => vars @ (all_vars pt)) [] ps
                | ConstructorP (_, pt) => all_vars pt
                | _ => []

        fun has_duplicate strings =
            case strings of
                [] => false
                | s::ss => 
                    (List.exists (fn x => x = s) ss) orelse (has_duplicate ss)
    in
        not (has_duplicate (all_vars p))
    end


(* Problem 11 *)
fun match (v, p) =
    case (v, p) of
        (_, Wildcard) => SOME []
        | (Unit, UnitP) => SOME []
        | (Const cv, ConstP cp) => if cv = cp then SOME [] else NONE
        | (value, Variable s)  => SOME [(s, value)]
        | (Tuple vs, TupleP ps) => 
            if List.length vs = List.length ps 
            then all_answers match (ListPair.zip(vs, ps))
            else NONE
        | (Constructor(sv, value), ConstructorP(sp, pt)) => if sv = sp then match(value, pt) else NONE
        | _ => NONE


fun first_match v ps =
    SOME (first_answer (fn p => match(v, p)) ps)
    handle NoAnswer => NONE
            


(* Challenge Problem *)
fun typecheck_patterns (_, []) = NONE
    | typecheck_patterns (datatype_list, pattern_list) =
        let
            fun type_contains (t1, t2) =
                case (t1, t2) of
                    (Anything, _) => true
                    | (UnitT, UnitT) => true
                    | (IntT, IntT) => true
                    | (Datatype s1, Datatype s2) => s1 = s2
                    | (TupleT ts1, TupleT ts2) =>
                        if List.length ts1 = List.length ts2
                        then foldl (fn ((x,y), b) => b andalso type_contains(x,y)) true (ListPair.zip(ts1, ts2))
                        else false
                    | _ => false



            fun type_of_pattern pt =
                case pt of
                    Wildcard => SOME Anything    
                    | Variable _ => SOME Anything
                    | UnitP => SOME UnitT
                    | ConstP _ => SOME IntT
                    | TupleP pts => (
                        case (all_answers (fn p => (
                                        case type_of_pattern p of
                                            NONE => NONE
                                            | SOME t => SOME [t])
                                    )
                                    pts) of 
                            NONE => NONE
                            | SOME type_list => SOME (TupleT type_list)
                        )
                    | ConstructorP (s, p) => 
                        let
                            fun type_match (cons, name, arg_type) (s, p) = 
                                if cons = s
                                then (
                                    case type_of_pattern p of
                                        NONE => false
                                        | SOME t => type_contains(arg_type, t))
                                else false
                        in
                            SOME (first_answer (fn (cons, name, arg_type) =>
                                            if type_match (cons, name, arg_type) (s, p)
                                            then SOME (Datatype name)
                                            else NONE)
                                          datatype_list)
                            handle NoAnswer => NONE 
                        end
        in
            let
                val types = List.map type_of_pattern pattern_list
                val first_type::_ = types
            in 
                foldl (fn (t, general) =>
                        case (t, general) of
                            (NONE, _) => NONE
                            | (_, NONE) => NONE
                            | (SOME t', SOME g') =>
                                if type_contains(g', t')
                                then t
                                else 
                                    if type_contains(t', g')
                                    then general
                                    else NONE
                        )
                        first_type
                        types                        
            end
        end

















