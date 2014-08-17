open Core.Std
open Util

let fact_norec fact n =
    if n <= 1 then
        Big_int.unit_big_int
    else
        Big_int.mult_int_big_int n (fact (n - 1))

let fact = memo_rec fact_norec

let sum_of_digits big_int =
    String.fold (Big_int.string_of_big_int big_int)
    ~init:0
    ~f:(fun acc chr -> acc + (Char.to_int chr) - (Char.to_int '0'))

let () = fact 100 |> sum_of_digits |> Printf.printf "%d\n"
