type date = int * int * int

(* Problem 1 *)
fun is_older ((y1, m1, d1), (y2, m2, d2)) =
    let 
        fun pair_less((x1, y1), (x2, y2)) =
            if x1 < x2
            then true
            else if x1 > x2
                 then false
                 else y1 < y2
    in
        if y1 < y2
        then true
        else if y1 > y2
             then false
             else pair_less((m1, d1), (m2, d2))

    end


(* Problem 2 *)
fun number_in_month (dates : date list, month) =
    if null dates
    then 0
    else 
        if (#2 (hd dates)) = month
        then 1 + number_in_month((tl dates), month)
        else number_in_month((tl dates), month)


(* Problem 3 *)
fun number_in_months (dates : date list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))


(* Problem 4 *)
fun dates_in_month (dates : date list, month : int) =
    if null dates
    then []
    else 
        let val d = hd dates
        in 
            if #2 d = month
            then d::dates_in_month((tl dates), month)
            else dates_in_month((tl dates), month)
        end


(* Problem 5 *)
fun dates_in_months (dates : date list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, (hd months)) @ (dates_in_months(dates, (tl months)))


(* Problem 6 *)
fun get_nth(xs, n) =
    if n = 1
    then hd xs
    else get_nth((tl xs), n - 1)



(* Problem 7 *)
fun date_to_string (y, m, d) =
    let 
        val month_names = ["January","February","March","April","May","June","July","August","September","October","November","December"]
    in
        get_nth(month_names, m) ^ " " ^ Int.toString(d) ^ ", " ^ Int.toString(y)
    end  



(* Problem 8 *)
fun number_before_reaching_sum (sum, xs) =
    let 
        fun aux (s, n, sub) =
            if s >= sum
            then n - 1
            else aux (s + (hd sub), n + 1, tl sub)
    in 

        aux (0, 0, xs) 
    end




(* Problem 9 *)
fun what_month day =
    let val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in number_before_reaching_sum (day, days_in_month) + 1
    end




(* Problem 10 *)
fun month_range (day1, day2) =
    if day1 > day2
    then []
    else (what_month day1) :: month_range(day1 + 1, day2)



(* Problem 11 *)
fun oldest dts =
    if null dts
    then NONE
    else 
        let 
            val sub_old = oldest (tl dts)
            val d = hd dts
        in 
            if isSome sub_old
            then 
                if is_older(d, (valOf sub_old))
                then SOME d
                else sub_old
            else
                SOME d
        end


(* Problem 12 *)
fun number_in_months_challenge (dates : date list, months : int list) =
    if null dates
    then 0
    else
        let 
            fun in_months (dt : date, ms) =
                if null ms
                then false
                else ((#2 dt) = (hd ms)) orelse in_months(dt, (tl ms))

            val dt = hd dates
        in 
            if in_months(dt, months)
            then 1 + number_in_months_challenge((tl dates), months)
            else number_in_months_challenge((tl dates), months)
        end



fun dates_in_months_challenge (dates : date list, months : int list) =
    if null dates
    then []
    else
        let 
            fun in_months (dt : date, ms) =
                if null ms
                then false
                else ((#2 dt) = (hd ms)) orelse in_months(dt, (tl ms))

            val dt = hd dates
        in 
            if in_months(dt, months)
            then dt :: dates_in_months_challenge((tl dates), months)
            else dates_in_months_challenge((tl dates), months)
        end



fun reasonable_date (y, m, d) =
    let
        val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        fun is_leap_year() = (y mod 400 = 0) orelse ((y mod 4) = 0 andalso (y mod 100) <> 0)
        fun reasonable_year() = y >= 1
        fun reasonable_month() = m >= 1 andalso m <= 12
        fun reasonable_day() =
            let val max_day = 
                if is_leap_year() andalso m = 2
                then get_nth(days_in_month, m) + 1
                else get_nth(days_in_month, m)
            in
                d >= 1 andalso d <= max_day
            end
    in

        reasonable_year() andalso reasonable_month() andalso reasonable_day()

    end

































