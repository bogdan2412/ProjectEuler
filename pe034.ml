open Core.Std
open Util

let fact_norec fact = function
    | n when n <= 1 -> 1
    | n -> n * fact (n - 1)

let fact = memo_rec fact_norec

let is_curious_number n =
    let rec iter sum = function
        | 0 -> n = sum
        | cur -> iter (sum + (fact (cur % 10))) (cur / 10)
    in
    iter 0 n

let () = fold_range 10 2_177_282 ~init:0 ~f:(fun acc v ->
             if is_curious_number v then
                 acc + v
             else
                 acc)
         |> Printf.printf "%d\n"
