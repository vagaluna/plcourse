(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)


(* Problem 1 *)

(* part a *)
fun all_except_option (s, []) = NONE
  | all_except_option (s, x::xs) =
        if same_string(s, x)
        then SOME xs
        else
            case all_except_option (s, xs) of
                NONE => NONE
              | SOME lst => SOME (x::lst)


(* part b *)
fun get_substitutions1 ([], s) = []
  | get_substitutions1 (sub::subs, s) =
        case all_except_option(s, sub) of
            NONE => get_substitutions1 (subs, s)
          | SOME lst => lst @ get_substitutions1 (subs, s)


(* part c *)
fun get_substitutions2 (substitutions, s) = 
    let
        fun helper (remain_subs, acc) = 
            case remain_subs of
                [] => acc
              | sub::subs => case all_except_option(s, sub) of
                                 NONE => helper (subs, acc)
                               | SOME lst => helper (subs, lst @ acc)

    in
        helper (substitutions, [])
    end


(* part d *)
fun similar_names (substitutions, {first = f, middle = m, last = l}) =
    let
        fun name_with_first fst = {first = fst, middle = m, last = l}
        fun helper [] = []
          | helper (fst::fsts) = (name_with_first fst) :: helper fsts
    in
        helper (f :: get_substitutions2 (substitutions, f))
    end



(* Problem 2 *)

(* part a *)
fun card_color (s : suit, _ : rank) =
    case s of
        Spades => Black
        | Clubs => Black
        | _ => Red


(* part b *)
fun card_value (_ : suit, r : rank) =
    case r of
        Num i => i
        | Ace => 11
        | _ => 10

(* part c *)
fun remove_card ([], _, e) = raise e
    | remove_card (c1::cs, c, e) =
        if c1 = c
        then cs
        else c1 :: (remove_card (cs, c, e))
    

(* part d *)
fun all_same_color [] = true
    | all_same_color (_::[]) = true
    | all_same_color (c1::c2::tail) = (card_color c1) = (card_color c2) andalso all_same_color(c2::tail)


(* part e *)
fun sum_cards cs =
    let
        fun acc (remain, sum) =
            case remain of
                [] => sum
                | c::tail => acc(tail, sum + card_value(c))
    in
        acc(cs, 0)
    end


(* part f *)
fun score (cs, goal) =
    let
        val sum = sum_cards cs
        val pre_score = if sum > goal
                        then 3 * (sum - goal)
                        else goal - sum
    in
        if all_same_color cs
        then pre_score div 2
        else pre_score
    end


(* part g *)
fun officiate (cards, moves, goal) =
    let
        fun step (helds, remain_cards, remain_moves, sum) = 
            case remain_moves of
                [] => helds
                | Draw::ms => (
                    case remain_cards of
                        [] => helds
                        | c::cs => 
                            let val s = sum + card_value c
                            in if s > goal
                               then c::helds
                               else step(c::helds, cs, ms, s)
                            end
                    )
                | (Discard c) :: ms =>
                    step(remove_card(helds, c, IllegalMove), remain_cards, ms, sum - (card_value c))

    in
        score(step([], cards, moves, 0), goal)
    end 




(* Problem 3 *)
 
(* part a *)
fun min_sum_and_count_ace (cs, (sum, count)) =
    case cs of
        [] => (sum, count)
        | (_, Ace)::cs' => min_sum_and_count_ace(cs', (sum + 1, count + 1))
        | c::cs' => min_sum_and_count_ace(cs', (sum + (card_value c), count))


fun score_challenge (cs, goal) =
    let
        fun score sum =
            let 
                val pre_score = if sum > goal
                                then 3 * (sum - goal)
                                else goal - sum
            in
                if all_same_color cs
                then pre_score div 2
                else pre_score
            end

        val (min_sum, ace_count) = min_sum_and_count_ace(cs, (0, 0))

        fun find_min_score (ace_remain, sum, min) =
            if sum > goal orelse ace_remain = 0
            then min
            else
                let val new_score = score(sum + 10)
                in 
                    find_min_score(
                        ace_remain - 1, 
                        sum + 10,
                        if new_score < min then new_score else min)
                end

    in
        find_min_score(ace_count, min_sum, score min_sum)
    end


fun officiate_challenge (cards, moves, goal) =
    let
        fun step (helds, remain_cards, remain_moves) = 
            case remain_moves of
                [] => helds
                | Draw::ms => (
                    case remain_cards of
                        [] => helds
                        | c::cs => 
                            let 
                                val helds2 = c::helds
                                val (min_sum, _) = min_sum_and_count_ace (helds2, (0, 0))
                            in 
                                if min_sum > goal
                                then helds2
                                else step(helds2, cs, ms)
                            end
                    )
                | (Discard c) :: ms =>
                    step(remove_card(helds, c, IllegalMove), remain_cards, ms)

    in
        score_challenge(step([], cards, moves), goal)
    end 

(*fun print_move m =
    case m of
        Draw => print "Draw\n"
        | (Discard c) => print ("Discard " ^ (Int.toString (card_value c)))*)

(* part b *)
fun careful_player (cards, goal) =
    let
        fun discard_below_goal (helds, g, sum) =
            case helds of
                [] => NONE
                | c::cs => 
                    let
                        val v = card_value c
                    in
                        if sum - v <= g
                        then SOME c
                        else discard_below_goal(cs, g - v, sum - v)
                    end


        fun should_draw(cards, helds, sum) =
            if sum < goal - 10
            then true
            else 
                case cards of
                    [] => false
                    | c::cs => sum + (card_value c) <= goal


        fun next_move(cards, helds) = 
            let
                val sum = sum_cards helds
            in
                if score(helds, goal) = 0
                then NONE
                else 
                    if should_draw(cards, helds, sum)
                    then SOME Draw
                    else
                        case cards of
                            [] => NONE
                            | (next_card::_) =>
                                case discard_below_goal(helds, goal - (card_value next_card), sum) of
                                    NONE => NONE
                                    | SOME c => SOME (Discard c)
                        
            end
            

        fun execute_move(m, cards, helds) =
            case (m, cards, helds) of
                (Draw, [], _) => NONE
                | (Draw, c::cs, _) => SOME (cs, c::helds)
                | (Discard c, _, _) => SOME (cards, remove_card(helds, c, IllegalMove))

    
        fun play(cards, helds, moves) =
            case next_move(cards, helds) of
                NONE => moves
                | SOME m => 
                    case execute_move(m, cards, helds) of
                        NONE => moves @ [m]
                        | SOME (new_cards, new_helds) => play(new_cards, new_helds, moves @ [m])

    in
        play(cards, [], [])
    end


























