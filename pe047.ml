open Core.Std

let prime_factor_count_upto upto =
    Common.generic_sieve upto
        ~init:(fun ~fill:_ ~set:_ -> ())
        ~update:(fun _ orig_cnt _ pos_cnt ->
            if orig_cnt = 0 then
                pos_cnt + 1
            else
                pos_cnt)

let prime_factor_count = prime_factor_count_upto 1_000_000

let rec iter n =
    if prime_factor_count       n = 4 &&
       prime_factor_count (n + 1) = 4 &&
       prime_factor_count (n + 2) = 4 &&
       prime_factor_count (n + 3) = 4 then
        n
    else
        iter (n + 1)

let () = iter 1 |> Printf.printf "%d"
