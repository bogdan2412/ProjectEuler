open Core.Std
open Util

let fact_norec fact n =
    if n < 2 then
        1
    else
        n * fact (n - 1)

let fact = memo_rec fact_norec

let get_nth_permutation cnt n =
    let rec iter cnt n digit used =
        if cnt = 0 then
            []
        else
            if Int.Set.mem used digit then
                iter cnt n (digit + 1) used
            else
                if fact (cnt - 1) < n then
                    iter cnt (n - (fact (cnt - 1))) (digit + 1) used
                else
                    digit :: (iter (cnt - 1) n 0 (Int.Set.add used digit))
    in
    iter cnt n 0 Int.Set.empty

let () = get_nth_permutation 10 1_000_000
         |> List.fold ~init:0 ~f:(fun acc v -> acc * 10 + v)
         |> Printf.printf "%d\n"
