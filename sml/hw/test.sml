datatype typ = Anything
         | UnitT
         | IntT
         | TupleT of typ list
         | Datatype of string

fun type_contains (t1, t2) =
                case (t1, t2) of
                    (Anything, _) => true
                    | (UnitT, UnitT) => true
                    | (IntT, IntT) => true
                    | (Datatype s1, Datatype s2) => s1 = s2
                    | (TupleT [], TupleT (t::ts)) => false
                    | (TupleT (t::ts), TupleT []) => false
                    | (TupleT ts1, TupleT ts2) =>
                        foldl (fn ((x,y), b) => b andalso type_contains(x,y)) true (ListPair.zip(ts1, ts2))
                    | _ => false