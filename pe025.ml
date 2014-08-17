open Core.Std
open Util

let fib_norec fib = function
    | n when n <= 0 -> Big_int.zero_big_int
    | 1 -> Big_int.unit_big_int
    | 2 -> Big_int.unit_big_int
    | n -> Big_int.add_big_int (fib (n - 2)) (fib (n - 1))

let fib = memo_rec fib_norec

let find_first_with_length_at_least at_least =
    let rec iter cur =
        if String.length (Big_int.string_of_big_int (fib cur)) >= at_least then
            cur
        else    
            iter (cur + 1)
    in
    iter 1

let () = find_first_with_length_at_least 1_000 |> Printf.printf "%d\n"
