open Core.Std
open Util

let is_sum_of_fifth_power_digits n =
    let pow5 n = let aux = n * n in aux * aux * n in
    let rec iter acc = function
        | 0 -> acc
        | n -> iter (acc + pow5 (n % 10)) (n / 10)
    in
    iter 0 n = n

let () = fold_range ~init:0 ~f:(fun acc v ->
            acc + (if is_sum_of_fifth_power_digits v then v else 0))
         2 1_000_000
         |> Printf.printf "%d\n"
